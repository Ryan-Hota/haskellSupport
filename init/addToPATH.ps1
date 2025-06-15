function Add-EnvPath {
  param(
      [Parameter(Mandatory=$true,HelpMessage='The Path to add to Users environment')]
      [string] $Path,

      [ValidateSet('Machine', 'User', 'Session')]
      [string] $Container = 'Session'
  )

  if ($Container -eq 'Session') {
      $envPaths = [Collections.Generic.List[String]]($env:Path -split ([IO.Path]::PathSeparator))
      if ($envPaths -notcontains $Path) {
          $envPaths.Add($Path)
          $env:PATH = $envPaths -join ([IO.Path]::PathSeparator)
      }
  }
  else {
      [Microsoft.Win32.RegistryHive]$hive, $keyPath = switch ($Container) {
          'Machine' { 'LocalMachine', 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment' }
          'User' { 'CurrentUser', 'Environment' }
      }

      $hiveKey = $envKey = $null
      try {
          $hiveKey = [Microsoft.Win32.RegistryKey]::OpenRemoteBaseKey($hive, '')
          $envKey = $hiveKey.OpenSubKey($keyPath, $true)
          $rawPath = $envKey.GetValue('PATH', '', 'DoNotExpandEnvironmentNames')

          $envPaths = [Collections.Generic.List[String]]($rawPath -split ([IO.Path]::PathSeparator))
          if ($envPaths -notcontains $Path) {
              $envPaths.Add($Path)
              $envKey.SetValue('PATH', ($envPaths -join ([IO.Path]::PathSeparator)), 'ExpandString')
          }
      }
      catch {
        $env:ADD_HASKELLSUPPORT_TO_PATH_FAILURE = "failed"
      }
      finally {
          if ($envKey) { $envKey.Close() }
          if ($hiveKey) { $hiveKey.Close() }
      }
  }
}

Add-EnvPath -Path ( $Env:HASKELLSUPPORT_EXECUTABLES_DIRECTORY_PATH ) -Container 'User'