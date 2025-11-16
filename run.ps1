function Show-Menu {
    Clear-Host
    Write-Host "==============================="
    Write-Host "   PROJECT EXECUTION MENU"
    Write-Host "==============================="
    Write-Host "1) Start backend - mainApi"
    Write-Host "2) Start backend - iartiApi"
    Write-Host "3) Start frontend (Angular)"
    Write-Host "4) Start ALL (one after another)"
    Write-Host "0) Exit"
    Write-Host "==============================="
}

function Run-MainApi {
    Write-Host "`nRunning mainApi..."
    Set-Location "./backend/mainApi"
    dotnet run
}

function Run-IartiApi {
    Write-Host "`nRunning iartiApi..."
    Set-Location "./backend/iartiApi"
    dotnet run
}

function Run-Frontend {
    Write-Host "`nRunning frontend (Angular)..."
    Set-Location "./frontend"
    ng serve
}

function Run-All {
    Write-Host "`nRunning mainApi..."
    Set-Location "./backend/mainApi"
    dotnet run

    Write-Host "`nRunning iartiApi..."
    Set-Location "../iartiApi"
    dotnet run

    Write-Host "`nRunning frontend..."
    Set-Location "../../frontend"
    ng serve
}

# ===== MAIN LOOP =====

while ($true) {
    Show-Menu
    $choice = Read-Host "Choose an option"

    switch ($choice) {
        "1" {
            Run-MainApi
            break
        }
        "2" {
            Run-IartiApi
            break
        }
        "3" {
            Run-Frontend
            break
        }
        "4" {
            Run-All
            break
        }
        "0" {
            Write-Host "Exiting..."
            exit
        }
        default {
            Write-Host "Invalid option. Press any key to try again..."
            $null = $Host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")
        }
    }
}
