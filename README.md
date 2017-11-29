# dotfiles
home folder backup using vcsh

Dotfiles managed by [vcsh](https://github.com/RichiH/vcsh/)

Workstation
```
vcsh clone -b git git@github.com:JonathanHuot/dotfiles.git git
```

Another combination of workstation
```
vcsh clone -b git-tr https://JonathanHuot@github.com/JonathanHuot/dotfiles.git git-tr
vcsh clone -b bash-tr https://JonathanHuot@github.com/JonathanHuot/dotfiles.git git-tr
```

Howto create new repo
```
vcsh init <repo>
vcsh <repo> checkout -b <repobranch>
vcsh <repo> add <files>
vcsh <repo> commit -m '<msg>'
vcsh <repo> remote add origin https://JonathanHuot@github.com/JonathanHuot/dotfiles.git
vcsh <repo> push -u origin <repobranch>
```
