// fdrawcmd.sys Demo Disk Utility by Simon Owen <simon@simonowen.com>
//
// Formats, reads, writes and verifies regular format disks (geometry setup below)
//
// Usage:
//
//  Format a disk:      DiskUtil.exe /format a:
//  Verify a disk:      DiskUtil.exe /verify a:
//  Write an image:     DiskUtil.exe disk.img a:
//  Create an image:    DiskUtil.exe a: disk.img
//  Format and write:   DiskUtil.exe disk.img a: /f
//  Format with verify: DiskUtil.exe /format a: /v

#include "DiskUtil.h"

#define DISK_SIDES          2       // sides per disk
#define DISK_TRACKS         80      // tracks per side
#define DISK_SECTORS        5       // sectors per track
#define DISK_DATARATE       2       // 2 is double-density
#define SECTOR_SIZE_CODE    3       // 0=128, 1=256, 2=512, 3=1024, ...
#define SECTOR_GAP3         0x20    // gap3 size between sectors
#define SECTOR_FILL         0xe5    // fill byte for formatted sectors
#define SECTOR_BASE         1       // first sector number on track
#define TRACK_SKEW          1       // format skew to the same sector on the next track

#define TRACK_SIZE          ((128<<SECTOR_SIZE_CODE)*DISK_SECTORS)

const unsigned char DPBdefault[] = {0xc3, 0x20, 0x00, 0x00, 0x53, 0x44, 0x43, 0x32,
                                    0x01, 0x01, 0x03, 0x01, 0x05, 0x00, 0x50, 0x00,
                                    0x28, 0x00, 0x04, 0x0f, 0x00, 0x84, 0x01, 0x7f,
						            0x00, 0xc0, 0x00, 0x20, 0x00, 0x04, 0x00, 0xd3};


///////////////////////////////////////////////////////////////////////////////

DWORD dwRet;

bool CmdRead (HANDLE h_, BYTE cyl_, BYTE head_, BYTE start_, BYTE count_, BYTE size_, PVOID pv_)
{
    FD_READ_WRITE_PARAMS rwp = { FD_OPTION_MFM, head_, cyl_,head_, start_,size_,start_+count_, 0x0a,0xff };
    return !!DeviceIoControl(h_, IOCTL_FDCMD_READ_DATA, &rwp, sizeof(rwp), pv_, count_*(128<<rwp.size), &dwRet, NULL);
}

bool CmdWrite (HANDLE h_, BYTE cyl_, BYTE head_, BYTE start_, BYTE count_, BYTE size_, PVOID pv_)
{
    FD_READ_WRITE_PARAMS rwp = { FD_OPTION_MFM, head_, cyl_,head_, start_,size_,start_+count_, 0x0a,0xff };
    return !!DeviceIoControl(h_, IOCTL_FDCMD_WRITE_DATA, &rwp, sizeof(rwp), pv_, count_*(128<<rwp.size), &dwRet, NULL);
}

bool CmdVerify (HANDLE h_, BYTE cyl_, BYTE head_, BYTE start_, BYTE end_, BYTE size_)
{
    FD_READ_WRITE_PARAMS rwp = { FD_OPTION_MFM, head_, cyl_,head_, start_,size_,end_, 0x0a,0xff };
    return !!DeviceIoControl(h_, IOCTL_FDCMD_VERIFY, &rwp, sizeof(rwp), NULL, 0, &dwRet, NULL);
}

bool CmdFormat (HANDLE h_, PFD_FORMAT_PARAMS pfp_, ULONG ulSize_)
{
    return !!DeviceIoControl(h_, IOCTL_FDCMD_FORMAT_TRACK, pfp_, ulSize_, NULL, 0, &dwRet, NULL);
}

bool SetDataRate (HANDLE h_, BYTE bDataRate_)
{
    return !!DeviceIoControl(h_, IOCTL_FD_SET_DATA_RATE, &bDataRate_, sizeof(bDataRate_), NULL, 0, &dwRet, NULL);
}

///////////////////////////////////////////////////////////////////////////////

HANDLE OpenFloppy (int nDrive_)
{
    char szDevice[32];
    wsprintf(szDevice, "\\\\.\\fdraw%u", nDrive_);

    HANDLE h = CreateFile(szDevice, GENERIC_READ|GENERIC_WRITE, 0, NULL, OPEN_EXISTING, 0, NULL);
    return (h != INVALID_HANDLE_VALUE && SetDataRate(h, DISK_DATARATE)) ? h : NULL;
}

HANDLE OpenImage (LPCSTR pcsz_, bool fWrite_)
{
    HANDLE h = fWrite_ ? CreateFile(pcsz_, GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, 0, NULL)
                       : CreateFile(pcsz_, GENERIC_READ, 0, NULL, OPEN_EXISTING, 0, NULL);

    return (h != INVALID_HANDLE_VALUE) ? h : NULL;
}

bool IsFloppy (LPCSTR pcsz_, int *pnDrive_=NULL)
{
    if (!isalpha(pcsz_[0]) || pcsz_[1] != ':' || pcsz_[2])
        return false;

    if (pnDrive_) *pnDrive_ = (pcsz_[0] & ~0x20) - 'A';
    return true;
}

DWORD GetDriverVersion ()
{
    DWORD dwVersion = 0;
    HANDLE h = CreateFile("\\\\.\\fdrawcmd", GENERIC_READ|GENERIC_WRITE, 0, NULL, OPEN_EXISTING, 0, NULL);

    if (h != INVALID_HANDLE_VALUE)
    {
        DeviceIoControl(h, IOCTL_FDRAWCMD_GET_VERSION, NULL, 0, &dwVersion, sizeof(dwVersion), &dwRet, NULL);
        CloseHandle(h);
    }

    return dwVersion;
}

void WriteCon (const char* pcsz_, ...)
{
    char sz[1024];

    va_list args;
    va_start(args, pcsz_);
    wvsprintf(sz, pcsz_, args);
    va_end(args);

    WriteFile(GetStdHandle(STD_OUTPUT_HANDLE), sz, lstrlen(sz), &dwRet, NULL);
}

char* LastError ()
{
    static char sz[256];
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS, NULL, GetLastError(),
                    MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), sz, sizeof(sz), NULL);
    CharToOem(sz, sz);
	return GetLastError() ? sz : "no error";
}

///////////////////////////////////////////////////////////////////////////////

bool ReadTrack (HANDLE h_, BYTE cyl_, BYTE head_, PBYTE pb_)
{
    return CmdRead(h_, cyl_, head_, SECTOR_BASE, DISK_SECTORS, SECTOR_SIZE_CODE, pb_);
}

bool WriteTrack (HANDLE h_, BYTE cyl_, BYTE head_, PBYTE pb_)
{
    return CmdWrite(h_, cyl_, head_, SECTOR_BASE, DISK_SECTORS, SECTOR_SIZE_CODE, pb_);
}

bool VerifyTrack (HANDLE h_, BYTE cyl_, BYTE head_)
{
    return CmdVerify(h_, cyl_, head_, SECTOR_BASE, SECTOR_BASE+DISK_SECTORS-1, SECTOR_SIZE_CODE);
}

bool FormatTrack (HANDLE h_, BYTE cyl_, BYTE head_)
{
    BYTE abFormat[sizeof(FD_FORMAT_PARAMS) + sizeof(FD_ID_HEADER)*DISK_SECTORS];

    PFD_FORMAT_PARAMS pfp = (PFD_FORMAT_PARAMS)abFormat;
    pfp->flags = FD_OPTION_MFM;
    pfp->phead = head_;
    pfp->size = SECTOR_SIZE_CODE;
    pfp->sectors = DISK_SECTORS;
    pfp->gap = SECTOR_GAP3;
    pfp->fill = SECTOR_FILL;

    PFD_ID_HEADER ph = pfp->Header;

    for (BYTE s = 0 ; s < pfp->sectors ; s++, ph++)
    {
        ph->cyl = cyl_;
        ph->head = head_;
        ph->sector = SECTOR_BASE + ((s + cyl_*(pfp->sectors - TRACK_SKEW)) % pfp->sectors);
        ph->size = pfp->size;
    }

    return CmdFormat(h_, pfp, (PBYTE)ph - abFormat);
}

///////////////////////////////////////////////////////////////////////////////

void Usage ()
{
    WriteCon("\nfdrawcmd.sys Demo Disk Utility by Simon Owen <simon@simonowen.com>\n"
		     "                           modified by Sergey A. <a-s-m@km.ru>\n"
             "\n"
             "Usage: DiskUtil <drive> <image>\n"
             "       DiskUtil <image> <drive> [/f] [/v]\n"
             "       DiskUtil /format <drive> [/v]\n"
             "       DiskUtil /verify <drive>\n"
             "\n"
             "  drive:  Floppy drive, typically A: or B:\n"
             "  image:  Disk image to create or write\n"
    );

    exit(EXIT_FAILURE);
}

int main (int argc_, char *argv_[])
{
    enum { cmdNone=0, cmdRead, cmdWrite, cmdFormat, cmdVerify };
    int nCommand = cmdNone, nDrive = -1;
    bool fFormat = false, fVerify = false;
    LPCSTR pcszFile = NULL;
    HANDLE h, hfile;
    BYTE cyl;

    for (int i = 1 ; i < argc_ ; i++)
    {
        if (!lstrcmpi(argv_[i], "/format") && nCommand == cmdNone)
            nCommand = cmdFormat;
        else if (!lstrcmpi(argv_[i], "/verify") && nCommand == cmdNone)
            nCommand = cmdVerify;
        else if (!lstrcmpi(argv_[i], "/f"))
            fFormat = true;
        else if (!lstrcmpi(argv_[i], "/v"))
            fVerify = true;
        else if (argv_[i][0] == '/')
            Usage();
        else if (nDrive == -1 && IsFloppy(argv_[i], &nDrive) && nCommand == cmdNone)
            nCommand = pcszFile ? cmdWrite : cmdRead;
        else if (!pcszFile && !IsFloppy(argv_[i]) && (pcszFile = argv_[i]) && nCommand == cmdNone)
            nCommand = (nDrive == -1) ? cmdWrite : cmdRead;
    }

    if (nCommand == cmdNone || nDrive == -1)    // require a command and a drive
        Usage();
    else if (!pcszFile && (nCommand == cmdRead || nCommand == cmdWrite))    // read/write require image file
        Usage();

    DWORD dwVersion = GetDriverVersion();

    if (!dwVersion)
        WriteCon("fdrawcmd.sys is not installed, see: http://simonowen.com/fdrawcmd/\n");
    else if (HIWORD(dwVersion) != HIWORD(FDRAWCMD_VERSION))
        WriteCon("The installed fdrawcmd.sys is not compatible with this utility.\n");
    else if (!(h = OpenFloppy(nDrive)))
        WriteCon("Failed to open floppy: %s\n", LastError());
    else if (!DeviceIoControl(h, IOCTL_FD_RESET, NULL, 0, NULL, 0, &dwRet, NULL))
        WriteCon("Failed to initialise controller: %s\n", LastError());
    else if (pcszFile && !(hfile = OpenImage(pcszFile, (nCommand == cmdRead))))
        WriteCon("Failed to open image: %s\n", LastError());
    else if (nCommand == cmdWrite && GetFileSize(hfile, NULL) != (TRACK_SIZE*DISK_TRACKS*DISK_SIDES))
        WriteCon("Image file is wrong size (should be %lu bytes)\n", TRACK_SIZE*DISK_TRACKS*DISK_SIDES);
    else
    {
        PBYTE pbTrack = (PBYTE)VirtualAlloc(NULL, TRACK_SIZE, MEM_COMMIT, PAGE_READWRITE);

        for (cyl = 0 ; cyl < DISK_TRACKS ; cyl++)
        {
            for (BYTE head = 0 ; head < DISK_SIDES ; head++)
            {
                if (!DeviceIoControl(h, IOCTL_FDCMD_SEEK, &cyl, sizeof(cyl), NULL, 0, &dwRet, NULL))
                    return false;

                switch (nCommand)
                {
                    case cmdFormat:
                        WriteCon("\rFormatting track %u side %u", cyl, head);
                        if (FormatTrack(h, cyl, head) && (!fVerify || VerifyTrack(h, cyl, head))) 
                            break;

                        WriteCon("\n\nFormat failed: %s\n", LastError());
                        return EXIT_FAILURE;

                    case cmdRead:
                        WriteCon("\rReading track %u side %u", cyl, head);
                        if (ReadTrack(h, cyl, head, pbTrack) && WriteFile(hfile, pbTrack, TRACK_SIZE, &dwRet, NULL))
                            break;

                        WriteCon("\n\nRead failed: %s\n", LastError());
                        return EXIT_FAILURE;

                    case cmdWrite:
                        WriteCon("\rWriting track %u side %u", cyl, head);
                        if (ReadFile(hfile, pbTrack, TRACK_SIZE, &dwRet, NULL) &&
                            (!fFormat || FormatTrack(h, cyl, head)) &&
                             WriteTrack(h, cyl, head, pbTrack) &&
                            (!fVerify || VerifyTrack(h, cyl, head)))
                            break;

                        WriteCon("\n\nWrite failed: %s\n", LastError());
                        return EXIT_FAILURE;

                    case cmdVerify:
                        WriteCon("\rVerifying track %u side %u", cyl, head);
                        if (VerifyTrack(h, cyl, head))
                            break;

                        WriteCon("\n\nVerify failed: %s\n", LastError());
                        return EXIT_FAILURE;
                }
            }
        }
        switch (nCommand)
        {
            case cmdFormat: WriteCon("\rWriting DPB                 ");
                            FillMemory(pbTrack, TRACK_SIZE, SECTOR_FILL);
                            CopyMemory(pbTrack, DPBdefault, 0x20);
                            cyl = 0;
                            if ( DeviceIoControl(h, IOCTL_FDCMD_SEEK, &cyl, sizeof(cyl), NULL, 0, &dwRet, NULL) &&
                                 WriteTrack(h, 0, 0, pbTrack) &&
                                 (!fVerify || VerifyTrack(h, 0, 0)))
                              WriteCon("\rFormat complete.           \n");
                            else
                              WriteCon("\rWriting DPB failed.        \n");
                            break;
            case cmdRead:   WriteCon("\rImage created successfully.\n");    break;
            case cmdWrite:  WriteCon("\rImage written successfully.\n");    break;
            case cmdVerify: WriteCon("\rDisk verified successfully.\n");    break;
        }
        if (pbTrack)
            VirtualFree(pbTrack, 0, MEM_RELEASE);

        return EXIT_SUCCESS;
    }

    return EXIT_FAILURE;
}
