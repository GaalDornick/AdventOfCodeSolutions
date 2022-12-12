
type Factor = number | [Worry, Worry]
class Worry {

    factors: Factor[]
    constructor(n: number| Factor[]) {
        if(typeof n ==="number") {
            this.factors = factorize(n)
        } else {
            this.factors = n
        }
    }
    mult(n: number) {
        this.factors.push(n)
    }
    square() {
        this.factors = [...this.factors, ...this.factors]
    }
    add(n: number) {
        this.factors = [[new Worry(this.factors), new Worry(n)]]
    }
    remainder(n: number) : number {
        return this.factors.map(factor => {
            if(typeof factor ==="number") {
                return factor%n
            } else {
                return (factor[0].remainder(n)+factor[1].remainder(n)) %n
            }
        }).reduce((t,f) => t*f, 1)
    }

}
function factorize(n: number): number[] {
    //console.log("Factorize" + n)
    if(n==1) return [1]
    if(n<=3) return [n,1]
    const sq_rt = Math.floor(Math.sqrt(n))
    for(let f = 2; f <=sq_rt; f++) {
        if(n%f==0) return [f, ...factorize(n/f)]
    }
    return [n,1]
}
class Monkey {
    items: Worry[]
    operation : (n: Worry) => void
    nextMonkey: (n: Worry) => number
    throws: number = 0
    constructor(_items: number[], _operation: (n: Worry) => void , _nextMonkey: (n: Worry) => number) {
        this.items = _items.map(item => new Worry(item))
        this.operation = _operation
        this.nextMonkey = _nextMonkey
    }

}
let puzzles = [
    [
        new Monkey([79, 98],
                   old => old.mult(19),
                   n => (n.remainder(23)==0)?2:3),

        new Monkey([54, 65, 75, 74],
                   old => old.add(6),
                   n => (n.remainder(19)==0)?2:0),

        new Monkey([79, 60, 97],
                   old => old.square(),
                   n => (n.remainder(13)==0)?1:3),

        new Monkey([74],
                   old => old.add(3),
                   n => (n.remainder(17)==0)?0:1),
    ],
    [
        new Monkey([83, 97, 95, 67],
                   old => old.mult(19),
                   n => (n.remainder(17)==0)?2:7),

        new Monkey([71, 70, 79, 88, 56, 70],
                   old => old.add(2),
                   n => (n.remainder(19)==0)?7:0),

        new Monkey([98, 51, 51, 63, 80, 85, 84, 95],
                   old => old.add(7),
                   n => (n.remainder(7)==0)?4:3),

        new Monkey([77, 90, 82, 80, 79],
                   old => old.add(1),
                   n => (n.remainder(11)==0)?6:4),

        new Monkey([68],
                   old => old.mult(5),
                   n => (n.remainder(13)==0)?6:5),

        new Monkey([60, 94],
                   old => old.add(5),
                   n => (n.remainder(3)==0)?1:0),

        new Monkey([81, 51, 85],
                   old => old.square(),
                   n => (n.remainder(5)==0)?5:1),

        new Monkey([98, 81, 63, 65, 84, 71, 84],
                   old => old.add(3),
                   n => (n.remainder(2)==0)?2:3),
    ]
]
{
    const monkeys = puzzles[1]
//puzzles.forEach(monkeys => {
    for(let turn = 0; turn<10000; turn++) {
        monkeys.forEach((monkey) => {
            for (const worry of monkey.items) {
                console.time("operation")
                monkey.operation(worry)
                if(turn==141||turn==1) {
                    console.timeLog("operation")
                }
                console.time("nextMonkey")
                const nextMonkeyIndex = monkey.nextMonkey(worry)
                if(turn==141||turn==1) {
                    console.timeLog("nextMonkey")
                    //console.log(JSON.stringify(monkey.items))
                    console.log(worry.toString() + " going to " + nextMonkeyIndex.toString())
                }
                monkeys[nextMonkeyIndex].items.push(worry)
            }
            monkey.throws = monkey.throws+monkey.items.length
            monkey.items = []

            //console.log(monkeys.map(m => m.items))
            //console.log("-----------------")
        })
    if(turn==141) console.log(monkeys.map(m => m.items))
        console.log("==============="+turn.toString())
//        console.log(monkeys.map(m => m.throws))

    }
    const throws = monkeys.map(m => m.throws).sort((a,b) => a-b)
    console.log(throws)
    console.log(throws[throws.length-1]*throws[throws.length-2])
    console.log("**********************")

}
//)