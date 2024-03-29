"use strict";
let scoresForPlay = new Map([
    ['R R', 1 + 3],
    ['R P', 2 + 6],
    ['R S', 3 + 0],
    ['P R', 1 + 0],
    ['P P', 2 + 3],
    ['P S', 3 + 6],
    ['S R', 1 + 6],
    ['S P', 2 + 0],
    ['S S', 3 + 3],
]);
let playMapping1 = new Map([
    ['A X', 'R R'],
    ['A Y', 'R P'],
    ['A Z', 'R S'],
    ['B X', 'P R'],
    ['B Y', 'P P'],
    ['B Z', 'P S'],
    ['C X', 'S R'],
    ['C Y', 'S P'],
    ['C Z', 'S S'],
]);
let playMapping2 = new Map([
    ['A X', 'R S'],
    ['A Y', 'R R'],
    ['A Z', 'R P'],
    ['B X', 'P R'],
    ['B Y', 'P P'],
    ['B Z', 'P S'],
    ['C X', 'S P'],
    ['C Y', 'S S'],
    ['C Z', 'S R'],
]);
//let opponentPlayMapping: Map<string, string> = new Map([
//    ['A', 'R'],
//    ['B', 'P'],
//    ['C', 'S'],
//])
//
//let playerPlayMapping: Map<string, string> = new Map([
//    ['X', 'R'],
//    ['Y', 'P'],
//    ['Z', 'S'],
//    ])
//let plays = [
//    ['A','Y'],
//    ['B','X'],
//    ['C','Z']
//]
let plays = [
    'C Z',
    'C Z',
    'C Y',
    'C Z',
    'C Z',
    'C Z',
    'A X',
    'A X',
    'C X',
    'C X',
    'A Z',
    'B X',
    'A X',
    'C Z',
    'A Z',
    'C Z',
    'C Z',
    'C Z',
    'A Z',
    'A X',
    'C Z',
    'C Z',
    'A Y',
    'A Y',
    'A X',
    'C Z',
    'A Y',
    'A Y',
    'C Z',
    'C Y',
    'C Y',
    'C Z',
    'A X',
    'A X',
    'A Y',
    'C Z',
    'A Y',
    'A Y',
    'A X',
    'A Z',
    'C Z',
    'A X',
    'C X',
    'A Y',
    'B Y',
    'B Y',
    'C Z',
    'A Z',
    'B X',
    'A Z',
    'B Z',
    'B Z',
    'A Y',
    'C Z',
    'C Z',
    'C X',
    'B Z',
    'C Z',
    'A Y',
    'C Z',
    'A X',
    'C X',
    'A X',
    'C Z',
    'A Y',
    'B X',
    'A Y',
    'C Z',
    'B Y',
    'C Z',
    'C Z',
    'A Y',
    'A Y',
    'A Y',
    'C Z',
    'B Y',
    'A X',
    'A Y',
    'C Z',
    'A Y',
    'C Z',
    'C Z',
    'C Z',
    'B X',
    'A Y',
    'B X',
    'C Z',
    'C Z',
    'B Y',
    'A X',
    'B Z',
    'C Z',
    'A X',
    'C Z',
    'B Y',
    'C Z',
    'A Y',
    'A Y',
    'A Z',
    'C Z',
    'A Y',
    'B Z',
    'A Z',
    'A Y',
    'A X',
    'A Y',
    'A X',
    'A Y',
    'A X',
    'C Z',
    'A Y',
    'B Z',
    'B Y',
    'A X',
    'C Z',
    'A X',
    'B Z',
    'A X',
    'A Y',
    'C Z',
    'A Z',
    'A X',
    'C X',
    'A Y',
    'A X',
    'A X',
    'A X',
    'B Y',
    'B Y',
    'A X',
    'C Z',
    'C Z',
    'A Y',
    'A X',
    'A Z',
    'C Z',
    'B Y',
    'B Z',
    'C X',
    'C Z',
    'A X',
    'A Y',
    'C Z',
    'A X',
    'C Y',
    'B X',
    'B Y',
    'A Y',
    'A Z',
    'A X',
    'A X',
    'C Y',
    'C Y',
    'A X',
    'A Z',
    'B Z',
    'C Y',
    'A Z',
    'A X',
    'A Y',
    'C X',
    'A X',
    'C Z',
    'C Z',
    'A Z',
    'B Y',
    'C Z',
    'A Z',
    'C Z',
    'B Z',
    'C Z',
    'C Y',
    'A X',
    'C Z',
    'C Z',
    'A Z',
    'C Z',
    'A X',
    'C Y',
    'A X',
    'C Z',
    'A Z',
    'A Y',
    'C Y',
    'B Z',
    'C Y',
    'C Y',
    'A Z',
    'A Y',
    'C Z',
    'C Z',
    'C Z',
    'C Z',
    'A Y',
    'A X',
    'C Z',
    'A Z',
    'C Z',
    'A Z',
    'C Z',
    'B Y',
    'C X',
    'A Y',
    'C Z',
    'A X',
    'A X',
    'C Z',
    'C X',
    'C X',
    'C Z',
    'A X',
    'C X',
    'A X',
    'B X',
    'A Y',
    'C Z',
    'C Z',
    'A Y',
    'C Y',
    'C Z',
    'A X',
    'A X',
    'A Z',
    'C X',
    'C Z',
    'A Y',
    'C Z',
    'C Z',
    'C Z',
    'C Z',
    'A Y',
    'C Z',
    'C Z',
    'C Z',
    'A Y',
    'A X',
    'A Y',
    'C Y',
    'A Y',
    'A Y',
    'A Y',
    'B Y',
    'C Y',
    'B X',
    'C X',
    'A Y',
    'C X',
    'C Y',
    'B X',
    'A Z',
    'C Z',
    'A X',
    'B X',
    'C Z',
    'C Z',
    'A Y',
    'A Y',
    'A Y',
    'A X',
    'C Z',
    'C Z',
    'C Z',
    'A X',
    'A Y',
    'A Y',
    'A Z',
    'B Y',
    'B Y',
    'A Z',
    'A Y',
    'A Y',
    'C X',
    'A Y',
    'A X',
    'A X',
    'A Z',
    'A X',
    'B X',
    'C Z',
    'B Y',
    'A Z',
    'C Z',
    'C Z',
    'C Z',
    'B Y',
    'A Y',
    'A Y',
    'A X',
    'B Z',
    'C Z',
    'C Y',
    'A X',
    'A Y',
    'A Y',
    'C X',
    'A X',
    'B Y',
    'A X',
    'A Y',
    'C X',
    'A X',
    'A Y',
    'C Z',
    'A X',
    'A X',
    'C Z',
    'C Z',
    'C Z',
    'C Z',
    'A Y',
    'A Y',
    'C Z',
    'A Y',
    'A X',
    'A Y',
    'B Z',
    'A X',
    'C Y',
    'C Z',
    'B X',
    'A X',
    'C Z',
    'A Z',
    'C Z',
    'A Z',
    'B Y',
    'A X',
    'C Z',
    'C X',
    'C Z',
    'C Z',
    'C Z',
    'C Z',
    'A X',
    'C Z',
    'C X',
    'C X',
    'C X',
    'C Z',
    'A Z',
    'C Z',
    'C Z',
    'C Z',
    'C Y',
    'A X',
    'C Y',
    'A X',
    'A Z',
    'A X',
    'C Z',
    'A X',
    'A Y',
    'B Y',
    'C Z',
    'A Y',
    'C Z',
    'C Z',
    'B Z',
    'A Z',
    'B Y',
    'A X',
    'C Z',
    'A Z',
    'C Z',
    'C Z',
    'A X',
    'C Z',
    'A X',
    'A Z',
    'A X',
    'A Z',
    'B Z',
    'B Z',
    'C Z',
    'C Z',
    'A X',
    'C Z',
    'C Z',
    'C Z',
    'A Y',
    'A X',
    'A X',
    'C X',
    'A Y',
    'C Z',
    'B Z',
    'C Z',
    'C X',
    'C Z',
    'C Z',
    'C Y',
    'A X',
    'A X',
    'A X',
    'B X',
    'C Z',
    'C Z',
    'A X',
    'C Z',
    'C Z',
    'A Y',
    'B Z',
    'A Z',
    'A Y',
    'A X',
    'C Z',
    'A Y',
    'A X',
    'C Z',
    'C X',
    'A Y',
    'A Y',
    'A Z',
    'C X',
    'A Y',
    'A Y',
    'C Z',
    'A Y',
    'C X',
    'A Y',
    'C X',
    'A X',
    'A X',
    'A X',
    'A Y',
    'C Y',
    'C Z',
    'B Z',
    'A X',
    'C Z',
    'A Y',
    'C Z',
    'C Z',
    'B Z',
    'A X',
    'A Z',
    'A Y',
    'B Y',
    'B X',
    'C Z',
    'B Y',
    'C Z',
    'A Y',
    'C Z',
    'B X',
    'B Y',
    'C Y',
    'A Y',
    'B X',
    'C Z',
    'A Y',
    'C Z',
    'A Y',
    'A X',
    'B X',
    'B Y',
    'A Y',
    'C X',
    'A Y',
    'A Y',
    'A X',
    'C Z',
    'A X',
    'B Y',
    'B X',
    'A Y',
    'B Z',
    'C X',
    'A Z',
    'A Z',
    'B Y',
    'A X',
    'A Z',
    'A Y',
    'B Z',
    'C Z',
    'A X',
    'A Z',
    'A X',
    'A Z',
    'A Y',
    'C Z',
    'C Z',
    'A Z',
    'C Z',
    'C Y',
    'A Y',
    'C Z',
    'A Y',
    'B Z',
    'C Z',
    'A X',
    'C Z',
    'C Y',
    'C Z',
    'C Z',
    'A X',
    'A Y',
    'A X',
    'C Z',
    'A X',
    'B X',
    'A Z',
    'C Y',
    'A Y',
    'B X',
    'A X',
    'A X',
    'C Y',
    'B Z',
    'A X',
    'A Y',
    'B X',
    'B Y',
    'A X',
    'B X',
    'A Y',
    'A X',
    'A Y',
    'A X',
    'A X',
    'A Z',
    'C Z',
    'A X',
    'C Z',
    'B Y',
    'A X',
    'A Z',
    'A Z',
    'B Y',
    'A Y',
    'C Z',
    'C X',
    'C Z',
    'C Z',
    'C Z',
    'B X',
    'A Y',
    'A X',
    'A X',
    'B Z',
    'C Z',
    'A X',
    'A X',
    'C X',
    'A X',
    'A X',
    'A Y',
    'B X',
    'A Z',
    'A Z',
    'C Z',
    'C Z',
    'B Z',
    'C Z',
    'B Y',
    'C Z',
    'C Z',
    'A Y',
    'B Z',
    'B Y',
    'C Z',
    'A X',
    'A Z',
    'A X',
    'C Z',
    'C Z',
    'A Y',
    'B X',
    'A X',
    'B Z',
    'A Y',
    'C Z',
    'A Z',
    'C Z',
    'A Z',
    'C Z',
    'A X',
    'A Y',
    'C Z',
    'A Y',
    'A X',
    'C Z',
    'A X',
    'C Z',
    'B X',
    'B Y',
    'C Y',
    'C X',
    'A Y',
    'C Z',
    'A Y',
    'A Y',
    'B Y',
    'B Y',
    'C X',
    'A X',
    'C Z',
    'B X',
    'A Y',
    'A Y',
    'A Y',
    'A Y',
    'B Y',
    'B X',
    'A X',
    'B Z',
    'A X',
    'B Z',
    'A X',
    'C Z',
    'B X',
    'C Z',
    'C Y',
    'C Z',
    'B Y',
    'A Z',
    'C Z',
    'C Z',
    'A Y',
    'C Z',
    'B X',
    'A Y',
    'B Z',
    'A Y',
    'A Y',
    'A X',
    'A Y',
    'C Z',
    'A Z',
    'A Y',
    'A X',
    'B Z',
    'B Y',
    'A X',
    'C Z',
    'A Y',
    'A Y',
    'A Z',
    'A X',
    'C Y',
    'C Z',
    'C Z',
    'C Z',
    'A Y',
    'A X',
    'A X',
    'C Z',
    'C Z',
    'A Y',
    'A Y',
    'A X',
    'B Z',
    'C X',
    'B Z',
    'C Z',
    'C X',
    'A X',
    'B Y',
    'C Z',
    'A X',
    'C Z',
    'C Z',
    'C Z',
    'A Z',
    'A X',
    'B Z',
    'C Y',
    'A X',
    'A X',
    'C Z',
    'C Z',
    'A Y',
    'B X',
    'C Y',
    'C X',
    'B Z',
    'C Z',
    'A Z',
    'A Z',
    'A X',
    'A Y',
    'C Z',
    'C Z',
    'A X',
    'A Y',
    'A Z',
    'A Y',
    'A Z',
    'B X',
    'B Y',
    'C Z',
    'A Z',
    'B Z',
    'A Y',
    'C Z',
    'B Z',
    'C Y',
    'C Z',
    'A Y',
    'A Y',
    'B X',
    'B Y',
    'C Z',
    'B X',
    'B X',
    'A Z',
    'A X',
    'A Y',
    'A X',
    'C Z',
    'C Y',
    'C Z',
    'A X',
    'A X',
    'C Z',
    'C Z',
    'A Z',
    'C Z',
    'C Z',
    'A Y',
    'A X',
    'C Z',
    'A Z',
    'A Y',
    'A Y',
    'A X',
    'A X',
    'A Y',
    'A X',
    'A Y',
    'C Z',
    'A X',
    'A X',
    'A Y',
    'C Z',
    'A Y',
    'B Y',
    'A X',
    'A Z',
    'A Y',
    'C Z',
    'A Y',
    'A Z',
    'C Z',
    'B Y',
    'A Z',
    'C Z',
    'A Z',
    'A X',
    'A X',
    'B Y',
    'C Z',
    'C Z',
    'A Z',
    'C Z',
    'A Y',
    'C Z',
    'B X',
    'A Z',
    'A Y',
    'A X',
    'A X',
    'A X',
    'B Y',
    'A X',
    'A Z',
    'A X',
    'C X',
    'A X',
    'A Z',
    'C Z',
    'A Y',
    'C Z',
    'A X',
    'A Y',
    'A X',
    'C Z',
    'A Y',
    'B Z',
    'B Y',
    'A X',
    'A X',
    'A Y',
    'C Z',
    'C Z',
    'C Z',
    'C Z',
    'B X',
    'C Y',
    'C Z',
    'A Y',
    'A X',
    'A X',
    'C Z',
    'A X',
    'C Z',
    'C Z',
    'A Y',
    'C Z',
    'A X',
    'A Y',
    'C Z',
    'B Y',
    'C Z',
    'C Z',
    'A Z',
    'C Y',
    'A Z',
    'A X',
    'A X',
    'A Y',
    'C X',
    'B Z',
    'C Z',
    'C X',
    'C Z',
    'B Y',
    'C Z',
    'A Y',
    'C Y',
    'C Z',
    'A Z',
    'C Z',
    'A Y',
    'B X',
    'A Y',
    'A Y',
    'A Y',
    'C Y',
    'B X',
    'A X',
    'B X',
    'A Y',
    'C Z',
    'C Z',
    'B Z',
    'A Z',
    'C Z',
    'A Z',
    'B Y',
    'A Y',
    'C Z',
    'C Z',
    'A X',
    'B Y',
    'B Z',
    'A X',
    'C Z',
    'A X',
    'A Y',
    'A Y',
    'A Z',
    'B X',
    'A Y',
    'A X',
    'A Z',
    'C Z',
    'C Z',
    'A X',
    'C Z',
    'C X',
    'C Z',
    'A X',
    'A X',
    'A X',
    'A Y',
    'C Z',
    'B Z',
    'A X',
    'B Z',
    'C Y',
    'A X',
    'B Z',
    'A Y',
    'A X',
    'C Z',
    'C Z',
    'C Z',
    'B Y',
    'C Z',
    'C Y',
    'B Y',
    'A Y',
    'A Y',
    'A X',
    'C X',
    'A X',
    'B X',
    'B Y',
    'A Y',
    'A X',
    'B X',
    'A X',
    'A X',
    'C Z',
    'C Y',
    'A Y',
    'A X',
    'C Z',
    'C Y',
    'C Z',
    'C Z',
    'A Z',
    'A Y',
    'A Y',
    'C X',
    'A X',
    'C Z',
    'A Y',
    'C Z',
    'A X',
    'B Y',
    'A X',
    'B X',
    'C Z',
    'C Z',
    'A Z',
    'A Y',
    'C Z',
    'C Z',
    'C Z',
    'B Z',
    'A Y',
    'C Z',
    'A X',
    'A Z',
    'A Y',
    'C Z',
    'A Z',
    'A Z',
    'A X',
    'A Y',
    'C Z',
    'C Z',
    'A Y',
    'C Y',
    'A Y',
    'B Z',
    'C Z',
    'C Y',
    'C Z',
    'A X',
    'C Z',
    'A Y',
    'A Z',
    'A X',
    'C Z',
    'A X',
    'A X',
    'A Y',
    'C Z',
    'A X',
    'A X',
    'A X',
    'C X',
    'C Z',
    'C Y',
    'B Z',
    'A Y',
    'A Y',
    'C Z',
    'C Z',
    'A Y',
    'C Z',
    'C Z',
    'C Z',
    'C Z',
    'C Z',
    'A Y',
    'B X',
    'B Z',
    'A X',
    'A Z',
    'B Y',
    'C Z',
    'C Z',
    'A Y',
    'A X',
    'A Y',
    'A Z',
    'C Z',
    'C Z',
    'A Y',
    'C Z',
    'A X',
    'C Y',
    'C Z',
    'A X',
    'A X',
    'C Z',
    'A X',
    'C Z',
    'A X',
    'A X',
    'C Z',
    'C X',
    'B Y',
    'C Z',
    'A Y',
    'C Y',
    'A Y',
    'A Y',
    'C Z',
    'C Z',
    'C Y',
    'A X',
    'A X',
    'C X',
    'A X',
    'A Z',
    'A X',
    'A Y',
    'C Z',
    'C Z',
    'A X',
    'B X',
    'B Z',
    'C Z',
    'C Z',
    'C Z',
    'C X',
    'C Y',
    'A X',
    'B Z',
    'A X',
    'A X',
    'B Y',
    'B X',
    'A Z',
    'A X',
    'A Y',
    'C Z',
    'A Y',
    'C Z',
    'A X',
    'A X',
    'B Z',
    'C Z',
    'C X',
    'A X',
    'A Y',
    'C Z',
    'C Y',
    'C Z',
    'A Y',
    'C Z',
    'A Y',
    'A Y',
    'A Z',
    'A Y',
    'C Z',
    'A Y',
    'C Z',
    'B Y',
    'C Z',
    'A X',
    'C Z',
    'C Y',
    'A Y',
    'A Y',
    'A Y',
    'C Y',
    'A Z',
    'A Y',
    'A Y',
    'C Z',
    'B X',
    'B Z',
    'A X',
    'C Z',
    'A Y',
    'C Z',
    'A Z',
    'A Z',
    'A Y',
    'A X',
    'C Y',
    'A X',
    'A Y',
    'C Z',
    'A X',
    'A Y',
    'A Y',
    'C Z',
    'A X',
    'C Z',
    'B X',
    'A Y',
    'A X',
    'C Z',
    'C Z',
    'C Z',
    'C Z',
    'B Y',
    'C Y',
    'A X',
    'C Z',
    'A X',
    'B X',
    'B Y',
    'A Y',
    'A X',
    'A X',
    'A X',
    'C Y',
    'A X',
    'A X',
    'A Y',
    'C X',
    'A X',
    'B X',
    'A Z',
    'C Z',
    'A Z',
    'A X',
    'C Z',
    'C Z',
    'B Y',
    'A Y',
    'A Y',
    'C Z',
    'C Z',
    'A Y',
    'B Y',
    'C Z',
    'A Y',
    'A X',
    'A X',
    'C Z',
    'C Z',
    'C Z',
    'A Z',
    'A Z',
    'A X',
    'C Z',
    'A X',
    'A X',
    'A X',
    'B Z',
    'B X',
    'C Z',
    'A X',
    'C Z',
    'C Z',
    'A X',
    'C Z',
    'A Y',
    'B Z',
    'B Z',
    'A X',
    'C X',
    'C Y',
    'A Y',
    'B Y',
    'A X',
    'C Z',
    'A Y',
    'C Z',
    'C Z',
    'B Z',
    'C Z',
    'C Y',
    'A Y',
    'C X',
    'A X',
    'C Y',
    'C Y',
    'C Z',
    'A Y',
    'B Y',
    'C Z',
    'C Y',
    'A X',
    'C X',
    'A Y',
    'B X',
    'C Y',
    'C X',
    'C Y',
    'C Z',
    'C Z',
    'B Z',
    'A Z',
    'A X',
    'A X',
    'C Z',
    'A X',
    'B Y',
    'C Y',
    'B Z',
    'A Y',
    'C Z',
    'A X',
    'A Y',
    'A Y',
    'C Z',
    'B Z',
    'A X',
    'A X',
    'A Y',
    'A Y',
    'A Y',
    'A X',
    'C Z',
    'A Y',
    'A Y',
    'C Y',
    'C Z',
    'A Y',
    'A X',
    'C Y',
    'A X',
    'A Y',
    'A X',
    'C Z',
    'A X',
    'A Z',
    'C Z',
    'A Y',
    'A Y',
    'C X',
    'A X',
    'C Z',
    'A Y',
    'A Y',
    'A Y',
    'A Z',
    'A X',
    'A Z',
    'C Z',
    'C Z',
    'A X',
    'C X',
    'A Z',
    'A Z',
    'C Z',
    'C Z',
    'C Y',
    'B X',
    'C Z',
    'A Y',
    'A Y',
    'B X',
    'B Y',
    'A Y',
    'B X',
    'A X',
    'C X',
    'B Y',
    'B X',
    'A X',
    'C Y',
    'B X',
    'A X',
    'C Y',
    'B X',
    'B Y',
    'C Z',
    'A X',
    'A Y',
    'A X',
    'C Y',
    'C Y',
    'C Z',
    'A Y',
    'A X',
    'B Z',
    'A Y',
    'C Z',
    'A Y',
    'B Z',
    'B Y',
    'B X',
    'B Z',
    'C X',
    'A Z',
    'C Z',
    'C Z',
    'B Y',
    'C Z',
    'C Z',
    'A X',
    'A Y',
    'A X',
    'A Y',
    'A Y',
    'B X',
    'A X',
    'A X',
    'A Z',
    'B Y',
    'A Y',
    'B Y',
    'A Y',
    'A X',
    'C Z',
    'A X',
    'B Y',
    'A Y',
    'C Z',
    'A Y',
    'A Y',
    'A Y',
    'C Z',
    'B Z',
    'C Z',
    'A X',
    'C Z',
    'A Z',
    'A Y',
    'A Z',
    'C Z',
    'C Z',
    'A Y',
    'A Y',
    'A X',
    'A X',
    'C Z',
    'C Z',
    'C X',
    'C Z',
    'B Y',
    'C Z',
    'A X',
    'C Z',
    'A Z',
    'C X',
    'C Y',
    'B Z',
    'C Z',
    'C Y',
    'A Y',
    'A X',
    'C Z',
    'A Z',
    'C Z',
    'B Z',
    'A Y',
    'A Y',
    'A Y',
    'C X',
    'A Y',
    'C Z',
    'A X',
    'A X',
    'A X',
    'A Y',
    'C Z',
    'C Z',
    'C Y',
    'A Y',
    'A Z',
    'A Y',
    'A Y',
    'C Y',
    'A X',
    'C Z',
    'A Y',
    'C Z',
    'C Z',
    'C Z',
    'C Z',
    'C Z',
    'B Z',
    'C Z',
    'C Z',
    'A X',
    'C Z',
    'A Y',
    'A Y',
    'B Y',
    'C Z',
    'B X',
    'B Y',
    'A Y',
    'A X',
    'C X',
    'A Z',
    'C Z',
    'C Z',
    'C Y',
    'A Y',
    'C Y',
    'C Z',
    'B X',
    'C Z',
    'A X',
    'C Z',
    'A Y',
    'A X',
    'A X',
    'A Y',
    'B Y',
    'C Y',
    'C Z',
    'C Z',
    'B Z',
    'C Z',
    'A Z',
    'A Y',
    'A X',
    'C Z',
    'A Y',
    'C Z',
    'C Z',
    'C Y',
    'A Y',
    'C Z',
    'B Y',
    'A Y',
    'A Y',
    'C X',
    'C Z',
    'A Z',
    'C X',
    'A X',
    'A Y',
    'C Z',
    'A Y',
    'A Y',
    'C X',
    'A X',
    'C Z',
    'B Y',
    'A Y',
    'A Z',
    'A Z',
    'A Y',
    'A X',
    'A Y',
    'A X',
    'C X',
    'B X',
    'C Y',
    'B Z',
    'A Z',
    'B X',
    'A Y',
    'B Y',
    'A X',
    'C Z',
    'C Z',
    'A Y',
    'C Y',
    'B Z',
    'B Y',
    'C Y',
    'A Z',
    'B X',
    'A X',
    'A X',
    'A X',
    'C X',
    'B X',
    'C Y',
    'A X',
    'C Z',
    'B Z',
    'C Y',
    'B X',
    'C Z',
    'C X',
    'A Y',
    'A X',
    'C Y',
    'B Z',
    'C Z',
    'B X',
    'B Y',
    'A X',
    'A Y',
    'A Y',
    'A Y',
    'B Y',
    'A X',
    'C X',
    'C Y',
    'A X',
    'A X',
    'C Z',
    'A X',
    'C Z',
    'C Z',
    'A X',
    'C Z',
    'C Z',
    'C Z',
    'A X',
    'C Z',
    'C Y',
    'C Z',
    'A Z',
    'A Y',
    'A Y',
    'C Z',
    'C X',
    'C Z',
    'C Z',
    'C X',
    'A X',
    'A Y',
    'B X',
    'C Y',
    'B Z',
    'B Y',
    'B Y',
    'C Z',
    'C Z',
    'A X',
    'C Z',
    'A Y',
    'C Z',
    'A X',
    'B Y',
    'A Y',
    'A X',
    'C Z',
    'C Y',
    'C Z',
    'A Y',
    'A X',
    'B Z',
    'B Z',
    'A Z',
    'A Y',
    'A X',
    'C Z',
    'B Z',
    'C X',
    'A Y',
    'C Z',
    'C Z',
    'A Z',
    'A Z',
    'C Z',
    'B X',
    'A Y',
    'A Y',
    'B X',
    'B Y',
    'A Y',
    'C Y',
    'C Z',
    'C Z',
    'C X',
    'A Y',
    'A Y',
    'A Z',
    'C Y',
    'B X',
    'B X',
    'A Y',
    'C Z',
    'C Z',
    'A Y',
    'A Y',
    'A Z',
    'C Z',
    'B Y',
    'A Y',
    'A Z',
    'A X',
    'C Z',
    'C Z',
    'B Y',
    'C Z',
    'C Z',
    'A Y',
    'C Z',
    'B Z',
    'B Z',
    'C Z',
    'B X',
    'A Z',
    'A Z',
    'A Y',
    'C Y',
    'B Z',
    'C Z',
    'C Z',
    'A X',
    'A X',
    'A X',
    'A Y',
    'A X',
    'A Y',
    'C Z',
    'A X',
    'C Z',
    'C Z',
    'C Z',
    'C Z',
    'A X',
    'A Y',
    'C Z',
    'A X',
    'A Y',
    'C Z',
    'A Y',
    'A Z',
    'B Y',
    'C Z',
    'A Y',
    'C Z',
    'A Y',
    'B X',
    'C Z',
    'A Y',
    'C Z',
    'C X',
    'C Z',
    'A Y',
    'B Z',
    'A X',
    'C Z',
    'C Y',
    'A Y',
    'B Z',
    'A Y',
    'B X',
    'A Y',
    'A X',
    'A X',
    'A Y',
    'A X',
    'A Y',
    'C Z',
    'A Y',
    'C Y',
    'A X',
    'A Z',
    'C Z',
    'A Z',
    'A Y',
    'A X',
    'A Y',
    'A X',
    'A Y',
    'A X',
    'B X',
    'A X',
    'A X',
    'C Y',
    'C Z',
    'A X',
    'A Y',
    'A X',
    'A Y',
    'C Z',
    'C Z',
    'A X',
    'A X',
    'B Z',
    'A Y',
    'C Z',
    'A Y',
    'B Y',
    'C Z',
    'C Z',
    'C Z',
    'C Z',
    'C Z',
    'C Z',
    'C Z',
    'C Z',
    'A Y',
    'C X',
    'B Y',
    'A X',
    'C Z',
    'A X',
    'A Z',
    'A Y',
    'B Y',
    'C X',
    'C Z',
    'C X',
    'B Y',
    'C Z',
    'A Y',
    'B Z',
    'C Z',
    'C Z',
    'A Y',
    'A X',
    'A Z',
    'C Z',
    'C Y',
    'A Y',
    'A X',
    'A X',
    'A Y',
    'C Z',
    'C Z',
    'B Y',
    'B Y',
    'C Z',
    'A X',
    'A Y',
    'B X',
    'B Y',
    'C Z',
    'A Z',
    'C Z',
    'A X',
    'B Y',
    'C Z',
    'C Z',
    'A X',
    'A Z',
    'A Y',
    'C Z',
    'B Y',
    'B Z',
    'C Y',
    'B X',
    'C Z',
    'A X',
    'A Y',
    'C Z',
    'C Z',
    'C Y',
    'A Y',
    'A Y',
    'A Y',
    'C X',
    'B Y',
    'A Y',
    'C Z',
    'C Z',
    'A X',
    'C Z',
    'A X',
    'C Z',
    'B Z',
    'C Z',
    'A Y',
    'A Y',
    'C Z',
    'C Z',
    'C Z',
    'A Y',
    'A Y',
    'B Z',
    'C Z',
    'A X',
    'C Y',
    'A X',
    'A Y',
    'A X',
    'A X',
    'C Z',
    'C Z',
    'C Z',
    'A Y',
    'C X',
    'B Z',
    'C Z',
    'C Y',
    'B X',
    'A Y',
    'A Y',
    'A X',
    'A X',
    'B Y',
    'C Z',
    'A Y',
    'A Z',
    'B Z',
    'A Z',
    'A X',
    'A Z',
    'A X',
    'A X',
    'C Z',
    'C X',
    'C Z',
    'B Y',
    'C X',
    'C Z',
    'C Z',
    'A X',
    'A X',
    'A Y',
    'A Z',
    'A Z',
    'A Y',
    'C Z',
    'A Z',
    'A X',
    'A Y',
    'A Y',
    'A Z',
    'A Y',
    'A X',
    'B X',
    'A Y',
    'A Y',
    'C Z',
    'C Z',
    'C Y',
    'A X',
    'B X',
    'B Y',
    'A Y',
    'C Z',
    'A X',
    'A Y',
    'B Z',
    'A Y',
    'C X',
    'B X',
    'C Z',
    'A Y',
    'C X',
    'C Y',
    'A Y',
    'A X',
    'A X',
    'C Z',
    'C Y',
    'A Z',
    'A X',
    'C Z',
    'C Z',
    'A Z',
    'A X',
    'C Y',
    'C Z',
    'A Y',
    'A Y',
    'C Z',
    'C Z',
    'C Z',
    'C Z',
    'A Y',
    'C Z',
    'C Z',
    'C Z',
    'B Z',
    'A Y',
    'C Z',
    'A X',
    'C Z',
    'A X',
    'A Y',
    'C Y',
    'C Z',
    'B Y',
    'A X',
    'A X',
    'C Z',
    'C Z',
    'A Y',
    'A X',
    'C Z',
    'A Y',
    'C Z',
    'C Z',
    'A X',
    'A Z',
    'A Y',
    'C Z',
    'A Y',
    'C Z',
    'B Y',
    'C X',
    'C Z',
    'A X',
    'A Y',
    'C Y',
    'A X',
    'A Y',
    'A Y',
    'A Z',
    'A Y',
    'C X',
    'A Y',
    'B Y',
    'C Z',
    'C Y',
    'B Z',
    'A Y',
    'B X',
    'A Y',
    'C Z',
    'A X',
    'A X',
    'C Z',
    'C Z',
    'A X',
    'A X',
    'A Z',
    'C Z',
    'C Z',
    'A Z',
    'A X',
    'B Y',
    'B Z',
    'B Z',
    'C Z',
    'A X',
    'C Z',
    'C Z',
    'A Y',
    'C Z',
    'C Z',
    'B Z',
    'A X',
    'B Z',
    'C Z',
    'C Y',
    'A Z',
    'C Z',
    'A Y',
    'A X',
    'B X',
    'A Z',
    'C Z',
    'A Z',
    'B X',
    'A Y',
    'A Y',
    'B Z',
    'C Z',
    'B Z',
    'C Z',
    'A X',
    'C Y',
    'A Y',
    'B Y',
    'B Y',
    'C Z',
    'C Y',
    'A Y',
    'A Z',
    'C Z',
    'A X',
    'A Y',
    'A Z',
    'A X',
    'A X',
    'C X',
    'C Z',
    'C Z',
    'A Z',
    'C Z',
    'A X',
    'C Z',
    'C Z',
    'A X',
    'A X',
    'A Z',
    'A X',
    'A X',
    'C X',
    'A X',
    'C Z',
    'C Z',
    'C Z',
    'A Z',
    'A X',
    'B X',
    'A Y',
    'A X',
    'B Z',
    'B Z',
    'C X',
    'A Y',
    'A X',
    'B Z',
    'C Z',
    'A Z',
    'C X',
    'A Y',
    'A X',
    'B Z',
    'C Z',
    'C Z',
    'B X',
    'B X',
    'A X',
    'A X',
    'A Y',
    'A Z',
    'A X',
    'A X',
    'A Y',
    'B Z',
    'C X',
    'A X',
    'A Y',
    'B Z',
    'C Z',
    'A X',
    'A Z',
    'C X',
    'C X',
    'B Z',
    'B Y',
    'C Y',
    'A Y',
    'B Z',
    'C Z',
    'A X',
    'C Z',
    'A Y',
    'C Z',
    'C Z',
    'A Y',
    'C Y',
    'A X',
    'A Y',
    'B Y',
    'C Y',
    'C Y',
    'A X',
    'A Y',
    'C Y',
    'A Z',
    'C Z',
    'A Z',
    'B Z',
    'A X',
    'A Y',
    'C Z',
    'A Y',
    'B Y',
    'A X',
    'C Z',
    'A X',
    'C Y',
    'A Y',
    'A X',
    'C Z',
    'A X',
    'C Y',
    'B X',
    'A X',
    'A Y',
    'A Y',
    'C X',
    'C Z',
    'A X',
    'A Z',
    'A Y',
    'C Z',
    'A Y',
    'C Z',
    'B X',
    'C Z',
    'C Z',
    'A X',
    'A X',
    'A X',
    'A Y',
    'C Z',
    'A Y',
    'A Y',
    'A Z',
    'A X',
    'C Y',
    'C Z',
    'A Z',
    'A Y',
    'C Z',
    'C Z',
    'B X',
    'A X',
    'C Y',
    'A X',
    'A X',
    'B Y',
    'B X',
    'C Z',
    'A X',
    'C Y',
    'A Y',
    'C Z',
    'B Y',
    'C Z',
    'A Y',
    'A X',
    'A Y',
    'C Z',
    'A Z',
    'C Z',
    'B Z',
    'A Y',
    'C Z',
    'A Z',
    'A X',
    'A X',
    'C Z',
    'C Z',
    'A X',
    'A X',
    'A Y',
    'C X',
    'A X',
    'B Z',
    'B Z',
    'B Z',
    'C Z',
    'C Z',
    'A X',
    'C Z',
    'C X',
    'A X',
    'C Z',
    'A Y',
    'C X',
    'C Z',
    'B Y',
    'B Z',
    'C Z',
    'A X',
    'C X',
    'A X',
    'C Y',
    'A Y',
    'B Y',
    'C Z',
    'A Z',
    'C X',
    'A Z',
    'A X',
    'A Z',
    'A X',
    'A Z',
    'C Y',
    'C Z',
    'A Y',
    'A Y',
    'A X',
    'B Y',
    'C Z',
    'B Y',
    'C Y',
    'C Z',
    'A Z',
    'C Z',
    'A Y',
    'B Z',
    'C Z',
    'A Z',
    'C Y',
    'A Z',
    'A Z',
    'A X',
    'A X',
    'C Y',
    'C Z',
    'A Z',
    'A Y',
    'B Y',
    'A Z',
    'C Z',
    'C Z',
    'B X',
    'C Z',
    'A X',
    'C Z',
    'B Z',
    'A Y',
    'A X',
    'B Y',
    'C X',
    'C Z',
    'A Z',
    'B X',
    'C Z',
    'C Z',
    'B Y',
    'B Z',
    'A Y',
    'A Y',
    'A X',
    'C Z',
    'A Z',
    'C Z',
    'A X',
    'C Z',
    'C Z',
    'A X',
    'A X',
    'A Y',
    'C Z',
    'B Y',
    'A X',
    'A Y',
    'A X',
    'C Z',
    'A Y',
    'A Y',
    'A Y',
    'C X',
    'C Z',
    'A X',
    'C Z',
    'B X',
    'A X',
    'A Z',
    'C Z',
    'A Y',
    'A Z',
    'A Z',
    'A X',
    'B Z',
    'A X',
    'C Z',
    'A Y',
    'B X',
    'C Z',
    'B Y',
    'B X',
    'C Z',
    'A X',
    'A Y',
    'A X',
    'A Z',
    'A Y',
    'A X',
    'C Y',
    'C Z',
    'B X',
    'C Y',
    'C Z',
    'B X',
    'A Z',
    'C Z',
    'C Z',
    'B Y',
    'B Z',
    'C Z',
    'C Z',
    'C Z',
    'C Z',
    'B Z',
    'B Y',
    'A Y',
    'C Z',
    'C Z',
    'B Y',
    'A Y',
    'C Z',
    'A Y',
    'C Z',
    'C Z',
    'C Z',
    'A Y',
    'B X',
    'A Z',
    'B Y',
    'C Z',
    'B Y',
    'C Z',
    'A Y',
    'B Z',
    'A X',
    'B Y',
    'B Y',
    'A Y',
    'B X',
    'C Z',
    'A Z',
    'C Y',
    'A X',
    'C Z',
    'C Z',
    'A X',
    'C Z',
    'A Y',
    'A X',
    'C Z',
    'A X',
    'A Z',
    'B Z',
    'C Z',
    'A Y',
    'A Y',
    'A X',
    'A Y',
    'C Z',
    'B Z',
    'C Z',
    'C Z',
    'C Z',
    'A Y',
    'C Y',
    'A Y',
    'A Y',
    'A X',
    'B Y',
    'A Y',
    'A Y',
    'B Z',
    'B Y',
    'C Z',
    'A Y',
    'A X',
    'A Y',
    'A Z',
    'A Y',
    'C Y',
    'C Z',
    'B Y',
    'A Y',
    'A X',
    'C X',
    'A Y',
    'C Z',
    'A Z',
    'C Z',
    'A X',
    'C Y',
    'A Y',
    'B Y',
    'B Z',
    'A Y',
    'C X',
    'B Y',
    'A Z',
    'A X',
    'A X',
    'A X',
    'A X',
    'A Y',
    'C Y',
    'A Y',
    'A X',
    'C Z',
    'A Y',
    'A X',
    'C Z',
    'B Y',
    'B X',
    'C Z',
    'B Z',
    'A Y',
    'A X',
    'C Z',
    'C Z',
    'A X',
    'C Z',
    'A Y',
    'C Z',
    'C Z',
    'C Z',
    'C X',
    'A Y',
    'A Y',
    'C Z',
    'C Z',
    'A X',
    'A X',
    'A Z',
    'A Y',
    'C Z',
    'A Z',
    'C Z',
    'C Z',
    'A X',
    'A Y',
    'A X',
    'C Y',
    'A Y',
    'A X',
    'A Y',
    'A X',
    'C Z',
    'C Y',
    'C Z',
    'C Y',
    'B Y',
    'A X',
    'A Y',
    'A Y',
    'B Z',
    'A Z',
    'C X',
    'C Z',
    'C Z',
    'A Y',
    'B X',
    'C Z',
    'A X',
    'A Y',
    'A Y',
    'A X',
    'A X',
    'C X',
    'B Y',
    'B X',
    'A X',
    'C Z',
    'A X',
    'A Y',
    'C Y',
    'A X',
    'A X',
    'B Z',
    'A X',
    'C Z',
    'A X',
    'A Y',
    'A Y',
    'C Z',
    'A Y',
    'A Y',
    'C Z',
    'A X',
    'A Y',
    'B Y',
    'C Z',
    'C X',
    'C X',
    'B X',
    'A Z',
    'A Y',
    'A X',
    'B X',
    'A Y',
    'A Z',
    'C Z',
    'A Z',
    'C X',
    'A Y',
    'C Z',
    'A Y',
    'A Y',
    'A X',
    'C Z',
    'C Z',
    'C Z',
    'C Z',
    'B X',
    'A X',
    'A Z',
    'A X',
    'A Y',
    'A X',
    'A X',
    'B Z',
    'C Z',
    'C X',
    'B Y',
    'A Y',
    'A X',
    'C X',
    'C Z',
    'B Z',
    'A Y',
    'B Z',
    'A X',
    'A X',
    'B Y',
    'A Z',
    'A Y',
    'A Y',
    'B X',
    'A Y',
    'A Y',
    'A Y',
    'C Z',
    'C Z',
    'B Z',
    'A Y',
    'A X',
    'A Y',
    'C Z',
    'C Z',
    'C Z',
    'A Z',
    'A Y',
    'B X',
    'A Y',
    'A Y',
    'A X',
    'A X',
    'C Z',
    'B Z',
    'C Z',
    'C Z',
    'C Z',
    'A Y',
    'C Y',
    'A Y',
    'C Z',
    'C Z',
    'B X',
    'C Z',
    'C Z',
    'C X',
    'A Y',
    'C Z',
    'A Y',
    'C Z',
    'A Y',
];
let scores1 = plays.map(p => playMapping1.get(p)).map(mp => scoresForPlay.get(mp));
let total1 = scores1.reduce((sum, score) => sum + score, 0);
console.log(total1);
let scores2 = plays.map(p => playMapping2.get(p)).map(mp => scoresForPlay.get(mp));
let total2 = scores2.reduce((sum, score) => sum + score, 0);
console.log(total2);
