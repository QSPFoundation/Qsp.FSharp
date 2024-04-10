cls

dotnet tool restore
dotnet paket install
dotnet fake build %*
