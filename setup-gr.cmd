/* Modula-2 installation fÅr OS/2 2.x or 3.x */
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
say "Modula-2 Setup fÅr OS/2 2.x oder 3.0    (c) 1995 by Juergen Neuhoff"
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
    say 'Installieren von Herkunft "' || origin || '" ? (J=ok N=sonstig)'
    pull answer
    if answer \= "J" then do
      if answer \= "N" then do
        iterate
      end
    end
    leave
  end
  if answer \= 'J' then do
    do forever
      origin = default
      say 'Bitte vollstÑndig eine andere Herkunft eintippen: "Laufwerk:\Pfad"'
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
      say "Laufwerk und/oder Pfad fÅr Herkunft ist ungÅltig oder unvollstÑndig"
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
  say 'Herkunft nicht ansprechbar: "' || origin || '"'
end
origindrive = substr( origin, 1, 2 )
say
say 'Modula-2 wird von folgender Herkunft installiert: "' || origin || '"'
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
    say 'Installieren auf das Ziel "' || dest || '" ? (J=ok N=woanders)'
    pull answer
    if answer \= "J" then do
      if answer \= "N" then do
        iterate
      end
    end
    leave
  end
  if answer \= 'J' then do
    do forever
      dest = default
      say 'Bitte eine vollstÑndige Zielangabe eintippen: "Laufwerk:\Pfad"'
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
      say "Laufwerk und/oder Pfad fÅr Zielangabe ist verkehrt oder unvollstÑndig"
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
        say '"' || dest || '" existiert schon, Åberschreiben? (J/N)'
        pull answer
        if answer \= "J" then do
          if answer \= "N" then do
            iterate
          end
        end
        leave
      end
      if answer = 'J' then do
        overwrite = 1
        leave
      end
      iterate
    end
    leave
  end
  say 'Ziel nicht zugreifbar: "' || dest || '"'
end
destdrive = substr( dest, 1, 2 )
say
say 'Modula-2 wird installiert nach: "' || dest || '"'
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
      say 'Soll bisheriges "' || destpath || '" vorher gelîscht werden? (J/N)'
      pull answer
      if answer \= "J" then do
        if answer \= "N" then do
          iterate
        end
      end
      leave
    end
    if answer = 'J' then do
      say
      say 'Lîsche altes "' || destpath || '"'
      say
      chdir filespec( "drive", dest ) || '\'
      rc = SysDestroyObject( dest )
      if rc \= 1 then do
        say 'Bisheriges Zielverzeichnis "' || destpath || '" kann nicht gelîscht werden'
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
  say 'Ziel-Laufwerk "' || destdrive || '" hat nicht ausreichend Platz'
  signal querydest
end


/* create destination directories if new */
if overwrite = 0 then do
  say
  say 'Erzeuge Zielverzeichnis(se) "' || dest || '"'
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
    say 'Zielverzeichnis "' || destpath || '" kann nicht erzeugt werden'
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
say 'Extrahiere Modula-2-Archiv nach "' || dest || '"'
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
say 'Erzeuge "' || dest || '\OS2BIN\MODENV.CMD" fÅr Modula-2-Umgebung'
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
  say '"' || modenv || '" in die "' || configsys || '" einfÅgen ? (J/N)'
  pull answer
  if answer \= "J" then do
    if answer \= "N" then do
      iterate
    end
  end
  leave
end
configos2 = dest || "\OS2BIN\CONFIG.OS2"
if answer = "J" then do
  call SysFileSearch ' MOD_', configsys, 'setfile.'
  if setfile.0 > 0 then do
    say 'Mîglicherweise existiert schon eine andere Modula-2-Umgebung'
    do forever
      say 'Trotzdem "' || modenv || '" in die "' || configsys || '" einfÅgen ? (J/N)'
      pull answer
      if answer \= "J" then do
        if answer \= "N" then do
          iterate
        end
      end
      leave
    end
  end
  if answer = "N" then do
    modenvold = dest || "\OS2BIN\MODENV.OLD"
    say 'Schreibe alte Angaben der Umgebung in "' || modenvold || '"'
    "ECHO REM --- Vermutlich alte Modula-2-Angaben aus CONFIG.SYS --- >" || modenvold
    if setfile.0 > 0 then do
      do i = 1 to setfile.0
        "ECHO " || setfile.i || " >>" || modenvold
      end
    end
    say
  end
end
configsys_changed = answer
if answer = "J" then do
  say '"FÅge jetzt "' || modenv || '" in die "' || configsys || '" ein'
  "COPY " || configsys || " " || configos2
  "COPY " || configos2 || "/A + " || modenv || " /A  " || configsys
end
else do
  say 'Ihre "' || configsys || '" wird nicht verÑndert'
end


/* Create a command file for restoring the OS/2 environment */
os2env = dest || "\OS2BIN\OS2ENV.CMD"
say
say 'Erzeuge eine Kommando-Datei "' || os2env || '"'
say
destdrive
chdir dest || "\OS2BIN"
"SAVEENV OS2ENV.CMD"


/* Installation done */
say
say 'Installation ist jetzt fertig.'
if configsys_changed = 'J' then do
  say 'Sie sollten den Rechner neu starten wegen Ihrer geÑnderten "CONFIG.SYS".'
end
else do
  say 'Ihre "' || configsys || '" ist nicht geÑndert worden.'
  say 'Sie sollten jedoch "' || modenv || '" immer dann starten,'
  say '  wenn Modula-2 zur Anwendung kommt.'
end
say 'Sie kînnen jederzeit die ursprÅngliche OS/2-Umgebung wiederherstellen,'
say '  indem Sie einfach "' || os2env '" aufrufen.'
say
say '-------------------------------------------------------------------'

exit
