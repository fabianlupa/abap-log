# Remove generation date footer so it does not show up as a diff
gci -Path ( split-path -parent $MyInvocation.MyCommand.Definition ) -Filter *.html -Recurse |
    %{
        $c = ($_ | Get-Content) 
        $c = $c -replace '<div id="footer">Generated on \d{2}.\d{2}.\d{4}</div>',""
        [IO.File]::WriteAllText($_.FullName, ($c -join "`r`n"))
    }
