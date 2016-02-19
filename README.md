# MoreCalculus

MoreCalculus` Mathematica package repository.

This package is meant to provide additional functions with application in calculus area. 

It's in a very crude state, released to get some feedback and test cases from community.


## Quick start

- Installing:

        Import[
            "https://raw.githubusercontent.com/kubaPod/MoreCalculus/master/install.m"
        ]
    
    This procedure will overwrite anything in `$UserBaseDirectory/Applications/MoreCalculus`
    
    or go to "release" tab, download the latest one and extract MoreCalculus folder
    in `$UserBaseDirectory/Applications`.

- When installed:

        << MoreCalculus`


##Example

Currently it contains only one function `DChange`, which helps transforming differential expressions to different coordinates.

For example, a wave equation in retarded/advanced coordinates:

    DChange[ 
        D[u[x, t], {t, 2}] == c^2 D[u[x, t], {x, 2}], 
        {a == x + c t, r == x - c t}, {x, t}, {a, r}, u[x, t]
    ]

>     c u^(1,1)[a,r]==0

More examples and comments in Mathematica.StackExchange topic where this idea originates from:


http://mathematica.stackexchange.com/q/80241/5478


