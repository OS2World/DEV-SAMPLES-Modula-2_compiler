/* Modula-2 installation for OS/2 2.x or 3.x */
/* trace all */

/* functions loaded from RexxUtil */
call RxFuncAdd 'SysFileTree', 'RexxUtil', 'SysFileTree'
call RxFuncAdd 'SysDriveInfo', 'RexxUtil', 'SysDriveInfo'
call RxFuncAdd 'SysMkDir', 'RexxUtil', 'SysMkDir'
call RxFuncAdd 'SysDestroyObject', 'RexxUtil', 'SysDestroyObject'
call RxFuncAdd 'SysGetMessage', 'RexxUtil', 'SysGetMessage'
call RxFuncAdd 'SysFileSearch', 'RexxUtil', 'SysFileSearch'

/* display introducing message */
"echo off"
say
say "Modula-2 setup for OS/2 2.x or 3.0      (c) 1995 by Juergen Neuhoff"
say "-------------------------------------------------------------------"
say

/* get default source path, usually A:\ */
parse upper source origin origin origin
origin = filespec( "drive", origin ) || filespec( "path", origin )
origin = delstr( origin, length( origin ) )
if (datatype( substr( origin, 1, 1 ) ) \= 'CHAR') then do
  origin = 'A:\'
end
else if (substr( origin, 2, 1 ) \= ':') then do
  origin = 'A:\'
end
else if (substr( origin, 3, 1 ) = '') then do
  origin = origin || '\'
end
else if (substr( origin, 3, 1 ) \= '\') then do
  origin = 'A:\'
end


/* let the user confirm or modify source path */
do forever
  default = origin
  do forever
    say 'Do you want to install from source "' || origin || '" ? (Y=ok N=other)'
    pull answer
    if answer \= "Y" then do
      if answer \= "N" then do
        iterate
      end
    end
    leave
  end
  if answer \= 'Y' then do
    do forever
      origin = default
      say 'Type in a complete source path "drive-letter:\path-name"'
      pull drivepath
      if datatype( substr( drivepath,1,1 ) ) = 'CHAR' then do
        if substr( drivepath, 2, 1 ) = ':' then do
          if substr( drivepath, 3, 1 ) = '\' then do
            if length( drivepath ) > 3 then do
              if right( drivepath, 1 ) \= '\' then do
                origin = drivepath
                leave
              end
            end
          end
          if substr( drivepath, 3, 1 ) = '' then do
            origin = drivepath
            leave
          end
        end
      end
      say "Source drive and/or path is invalid or incomplete"
    end
  end
  rc = SysFileTree( origin, origin_filetree, 'D' )
  if rc = 0 then do
    if origin_filetree.0 > 0 then do
      leave
    end
    if substr( origin, 2, 2 ) = ':\' then do
      if substr( origin, 4, 1 ) = '' then do
        /* installing from a main directory */
        leave
       end
    end
  end
  say 'Cannot access source "' || origin || '"'
end
origindrive = substr( origin, 1, 2 )
say
say 'Modula-2 will be installed from "' || origin || '"'
say


/* get default target path from OS/2 drive, e.g. C:\MOD32 */
dest = value( 'system_ini',,'os2environment' )
dest = substr( dest, 1, 2 )
os2drive = dest
dest = os2drive || "\MOD32"
default = dest


/* let the user confirm or change the default target */
querydest:
do forever
  dest = default
  do forever
    say 'Do you want to install to destination "' || dest || '" ? (Y=ok N=other)'
    pull answer
    if answer \= "Y" then do
      if answer \= "N" then do
        iterate
      end
    end
    leave
  end
  if answer \= 'Y' then do
    do forever
      dest = default
      say 'Type in a complete destination path "drive-letter:\path-name"'
      pull drivepath
      if drivepath = '' then do
        leave
      end
      if datatype( substr( drivepath,1,1 ) ) = 'CHAR' then do
        if substr( drivepath, 2, 1 ) = ':' then do
          if substr( drivepath, 3, 1 ) = '\' then do
            if length( drivepath ) > 3 then do
              if right( drivepath, 1 ) \= '\' then do
                dest = drivepath
                leave
              end
            end
          end
        end
      end
      say "Destination drive and/or path is invalid or incomplete"
    end
  end
  if dest = '' then do
    iterate
  end
  overwrite = 0
  rc = SysFileTree( dest, dest_filetree, 'D' )
  if rc = 0 then do
    if dest_filetree.0 > 0 then do
      do forever
        say
        say '"' || dest || '" already exists, overwrite it? (Y/N)'
        pull answer
        if answer \= "Y" then do
          if answer \= "N" then do
            iterate
          end
        end
        leave
      end
      if answer = 'Y' then do
        overwrite = 1
        leave
      end
      iterate
    end
    leave
  end
  say 'Cannot access destination "' || dest || '"'
end
destdrive = substr( dest, 1, 2 )
say
say 'Modula-2 will be installed to "' || dest || '"'
say


/* delete old destination path if there and if not needed */
if overwrite = 1 then do
  if filespec( "name", dest ) \= '' then do
    i = 3;
    do forever
      j = pos( '\', dest, i+1 )
      if j == 0 then do
        j = length( dest )
        destpath = substr( dest, i+1, j-i )
        leave
      end
      i = j
    end
    do forever
      say 'Delete old "' || destpath || '" before installation? (Y/N)'
      pull answer
      if answer \= "Y" then do
        if answer \= "N" then do
          iterate
        end
      end
      leave
    end
    if answer = 'Y' then do
      say
      say 'Deleting old "' || destpath || '"'
      say
      chdir filespec( "drive", dest ) || '\'
      rc = SysDestroyObject( dest )
      if rc \= 1 then do
        say 'Old destination "' || destpath || '" cannot be deleted'
        exit
      end
      overwrite = 0
    end
  end
end

/* check disk space before installation */
driveinfo = SysDriveInfo( destdrive )
parse upper var driveinfo    drive free total label
if free < 3000000 then do
  say 'Destination drive "' || destdrive || '" has not enough free space'
  signal querydest
end


/* create destination directories if new */
if overwrite = 0 then do
  say
  say 'Creating destination directory path "' || dest || '"'
  say
  i = 3
  if right( dest, 1 ) \= '\' then do
    dest = dest || '\'
  end
  do forever
    i = pos( "\", dest, i+1 )
    if (i == 0) then do
      leave
    end
    destpath = substr( dest, 1, i-1 )
    if (SysFileTree( destpath, dest_filetree, 'D' ) == 0) then do
      if dest_filetree.0 > 0 then do
        iterate
      end
    end
    rc = SysMkDir( destpath )
    if rc \= 0 then do
      destcreated = 0
      leave
    end
    destcreated = 1
  end
  if destcreated == 0 then do
    say 'Destination directory "' || destpath || '" cannot be created'
    say SysGetMessage( rc )
    exit
  end
  if right( dest, 1 ) == '\' then do
    if right( dest, 2 ) \= ':\' then do
      dest = substr( dest, 1, length( dest )-1 )
    end
  end
end



/* now do the installation */
say
say 'Extracting Modula-2 archive to "' || dest || '"'
say
chdir origin
chdir dest
destdrive
if overwrite = 1 then do
  if right( origin, 1 ) \= '\' then do
    origin || '\' || "modula2.exe -fo"
    origin || '\' || "modula2.exe -un"
  end
  else do
    origin || "modula2.exe -fo"
    origin || "modula2.exe -un"
  end
end
else do
  if right( origin, 1 ) \= '\' then do
    origin || '\' || "modula2.exe"
  end
  else do
    origin || "modula2.exe"
  end
end
os2obj = dest || "\os2obj"
os2obj_exist = 0
rc = SysFileTree( os2obj, os2obj_filetree, 'D' )
if rc = 0 then do
  if os2obj_filetree.0 > 0 then do
    os2obj_exist = 1
  end
end
if os2obj_exist = 0 then do
  mkdir os2obj
end

/* now create a command file for Modula-2 environment */
say
say 'Creating "' || dest || '\OS2BIN\MODENV.CMD" for Modula-2 environment'
say
mod_src = ".;"    || dest || "\OS2MOD;"
mod_src = mod_src || dest || "\OS2API;"
mod_src = mod_src || dest || "\OS2SRC;"
mod_src = mod_src || dest || "\OS2DEMO\OOPS;"
mod_src = mod_src || dest || "\OS2DEMO\HELLO;"
mod_src = mod_src || dest || "\OS2DEMO\ANIMALS;"
mod_tmp = dest
lib = value( 'LIB',,'OS2ENVIRONMENT' )
mod_lib = ""
if pos( ".", lib ) = 0 then do
  mod_lib = ".;"
end
mod_lib = mod_lib || dest || "\OS2LIB;" || lib
;dpath = value( 'DPATH',,'OS2ENVIRONMENT' )
;mod_obj_flat32 = dest || "\OS2OBJ;" || dpath
mod_obj_flat32 = dest || "\OS2OBJ;"
mod_obj_small = ".;"
mod_obj_medium = ".;"
mod_obj_compact = ".;"
mod_obj_large = ".;"
path = value( 'PATH',,'OS2ENVIRONMENT' )
if pos( dest || "\OS2BIN;", path ) = 0 then do
  path = dest || "\OS2BIN;" || path
end
tmp = value( 'TMP',,'OS2ENVIRONMENT' )
if tmp = "" then do
  tmp = mod_tmp
end
modenv = dest || "\OS2BIN\MODENV.CMD"
"ECHO REM --- Modula-2 settings --- "           || ">"  || modenv
"ECHO SET MOD_SRC="         || mod_src          || ">>" || modenv
"ECHO SET MOD_TMP="         || mod_tmp          || ">>" || modenv
"ECHO SET MOD_LIB="         || mod_lib          || ">>" || modenv
"ECHO SET MOD_OBJ_FLAT32="  || mod_obj_flat32   || ">>" || modenv
"ECHO SET MOD_OBJ_SMALL="   || mod_obj_small    || ">>" || modenv
"ECHO SET MOD_OBJ_MEDIUM="  || mod_obj_medium   || ">>" || modenv
"ECHO SET MOD_OBJ_COMPACT=" || mod_obj_compact  || ">>" || modenv
"ECHO SET MOD_OBJ_LARGE="   || mod_obj_large    || ">>" || modenv
"ECHO SET PATH="            || path             || ">>" || modenv
"ECHO SET TMP="             || tmp              || ">>" || modenv


/* update CONFIG.SYS if requested */
configsys = os2drive || "\CONFIG.SYS"
do forever
  say 'Append "' || modenv || '" to "' || configsys || '" ? (Y/N)'
  pull answer
  if answer \= "Y" then do
    if answer \= "N" then do
      iterate
    end
  end
  leave
end
configos2 = dest || "\OS2BIN\CONFIG.OS2"
if answer = "Y" then do
  call SysFileSearch ' MOD_', configsys, 'setfile.'
  if setfile.0 > 0 then do
    say 'Another Modula-2 environment may already exist'
    do forever
      say 'Append "' || modenv || '" to "' || configsys || '" anyhow ? (Y/N)'
      pull answer
      if answer \= "Y" then do
        if answer \= "N" then do
          iterate
        end
      end
      leave
    end
  end
  if answer = "N" then do
    modenvold = dest || "\OS2BIN\MODENV.OLD"
    say 'Putting old settings into "' || modenvold || '"'
    "ECHO REM --- Possible old Modula-2 settings from CONFIG.SYS --- >" || modenvold
    if setfile.0 > 0 then do
      do i = 1 to setfile.0
        "ECHO " || setfile.i || " >>" || modenvold
      end
    end
    say
  end
end
configsys_changed = answer
if answer = "Y" then do
  say '"Appending "' || modenv || '" to your "' || configsys || '"'
  "COPY " || configsys || " " || configos2
  "COPY " || configos2 || "/A + " || modenv || " /A  " || configsys
end
else do
  say 'Your "' || configsys || '" will not be changed'
end


/* Create a command file for restoring the OS/2 environment */
os2env = dest || "\OS2BIN\OS2ENV.CMD"
say
say 'Creating a command file "' || os2env || '"'
say
destdrive
chdir dest || "\OS2BIN"
"SAVEENV OS2ENV.CMD"


/* Installation done */
say
say 'Installation is now completed.'
if configsys_changed = 'Y' then do
  say 'Shut down and reboot your system because of your updated "CONFIG.SYS"'
end
else do
  say 'Your "' || configsys || '" has not been changed'
  say 'However, you''ll have to run "' || modenv || '"'
  say '  whenever using Modula-2'
end
say 'You can always restore the original OS/2 environment by running'
say '  "' || os2env '"'
say
say '-------------------------------------------------------------------'

exit
