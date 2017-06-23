vhd-info-

http://www.altaro.com/hyper-v/install-hyper-v-powershell-module/

管理者権限で以下を実行
> Enable-WindowsOptionalFeature -Online -FeatureName Microsoft-Hyper-V-Management-PowerShell

c:\projs\edk2>"c:\Program Files\Oracle\VirtualBox\VBoxManage.exe" showhdinfo c:\edk2\hello.vhd
UUID:           677304bc-c022-4562-85fe-28cd687db3d2
Parent UUID:    base
State:          locked write
Type:           normal (base)
Location:       C:\edk2\hello.vhd
Storage format: VHD
Format variant: dynamic default
Capacity:       200 MBytes
Size on disk:   18 MBytes
Encryption:     disabled
In use by VMs:  hello (UUID: 9bd05bc6-6628-41fd-8380-77ab23a7833a)
