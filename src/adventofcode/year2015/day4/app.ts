import {Md5} from 'ts-md5';

let puzzles = [
    'abcdef',
    'pqrstuv',
    'bgvyzdsv'
]

function isMineable1(key: string) : boolean {
    const hash = Md5.hashStr(key)
    //console.log(key + '-' + hash)
    return hash.startsWith('00000')
}

function isMineable2(key: string) : boolean {
    const hash = Md5.hashStr(key)
    //console.log(key + '-' + hash)
    return hash.startsWith('000000')
}

puzzles.forEach(secret => {
    let num = 1
    while(!isMineable1(secret+num.toString())) {
        num++
    }
    console.log(num)
    let num2= 1
    while(!isMineable2(secret+num2.toString())) {
        num2++
    }
    console.log(num2)
})