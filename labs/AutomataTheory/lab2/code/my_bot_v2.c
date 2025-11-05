#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ROCK 1
#define PAPER 2
#define SCISSORS 3

#define WIN 1
#define LOSE -1
#define DRAW 0

#define MAX_HISTORY 1000
#define MAX_PATTERN_LENGTH 10
#define LOSE_STREAK_THRESHOLD 5  // СМЕНА СТРАТЕГИИ ТОЛЬКО ПРИ 5 ПОРАЖЕНИЯХ ПОДРЯД

typedef struct {
    int opponentMoves[MAX_HISTORY];
    int ourMoves[MAX_HISTORY];
    int outcomes[MAX_HISTORY];
    int length;
} THistory;

THistory history;
int current_round = 0;
int current_lose_streak = 0;
int current_strategy = 1;  // 1 = стандартная, 2 = модифицированная

int beats(int move) {
    switch (move) {
        case ROCK:     return PAPER;
        case PAPER:    return SCISSORS;
        case SCISSORS: return ROCK;
        default:       return PAPER;
    }
}

int getCounterMove(int predictedMove) { return beats(predictedMove); }

int determineOutcome(int ourMove, int opponentMove) {
    if (ourMove == opponentMove) return DRAW;
    if ((ourMove == ROCK && opponentMove == SCISSORS) ||
        (ourMove == PAPER && opponentMove == ROCK) ||
        (ourMove == SCISSORS && opponentMove == PAPER))
        return WIN;
    return LOSE;
}

/* ============= УСИЛЕННЫЙ ПРОГНОЗ: ТОЧНОСТЬ + АДАПТАЦИЯ ============= */

/**
 * Анализ 1: ВЗАИМОДЕЙСТВИЕ - ищем паттерны "наш ход + ход противника"
 * УСКОРЕНИЕ: ищем только в последних 5 парах
 */
int analyzeInteractionPatterns(int maxLen) {
    if (history.length < maxLen + 1) return -1;
    int search_start = (history.length > 5) ? history.length - 5 : 0;
    for (int len = maxLen; len >= 2; len--) {
        if (history.length < len + 1) continue;
        int cur_start = history.length - len;
        for (int i = search_start; i < history.length - len; i++) {
            int match = 1;
            for (int j = 0; j < len; j++) {
                if (history.ourMoves[i + j] != history.ourMoves[cur_start + j] ||
                    history.opponentMoves[i + j] != history.opponentMoves[cur_start + j]) {
                    match = 0; break;
                }
            }
            if (match) return history.opponentMoves[i + len];
        }
    }
    return -1;
}

/**
 * Анализ 2: РЕАКЦИЯ НА ИСХОД
 */
int analyzeOutcomeReaction() {
    if (history.length < 3) return -1;
    int last_outcome = history.outcomes[history.length - 2];
    int last_opp = history.opponentMoves[history.length - 2];
    if (last_outcome == -WIN) return beats(last_opp);  // Он проиграл → сменит
    if (last_outcome == WIN)  return last_opp;         // Он выиграл → повторит
    return -1;
}

/**
 * Анализ 3: ПАТТЕРНЫ ПРОТИВНИКА
 */
int analyzePatterns(int maxLen) {
    if (history.length < maxLen + 1) return -1;
    for (int len = maxLen; len >= 2; len--) {
        if (history.length < len + 1) continue;
        int cur_start = history.length - len;
        for (int i = len; i < history.length - len; i++) {
            int match = 1;
            for (int j = 0; j < len; j++) {
                if (history.opponentMoves[i + j] != history.opponentMoves[cur_start + j]) {
                    match = 0; break;
                }
            }
            if (match) return history.opponentMoves[i + len];
        }
    }
    return -1;
}

/**
 * Анализ 3.5: ПОСЛЕДНИЙ ХОД ПРОТИВНИКА (если ничего нет)
 */
int predictLastMove() {
    if (history.length < 2) return -1;
    return history.opponentMoves[history.length - 1];
}

/**
 * Анализ 4: ЧАСТОТА — УСИЛЕННАЯ ВЕРСИЯ
 * Последние 3 хода — вес ×2
 */
int analyzeFrequency() {
    if (history.length < 2) return -1;
    double r = 0, p = 0, s = 0;
    int window = (history.length < 20) ? history.length : 20;
    int start = history.length - window;

    for (int i = start; i < history.length; i++) {
        double w = 0.5 + (i - start) / (double)window;
        if (i >= history.length - 3) w *= 2.0;  // УСИЛЕНИЕ ПОСЛЕДНИХ 3
        switch (history.opponentMoves[i]) {
            case ROCK:     r += w; break;
            case PAPER:    p += w; break;
            case SCISSORS: s += w; break;
        }
    }
    if (r >= p && r >= s) return ROCK;
    if (p >= s) return PAPER;
    return SCISSORS;
}

/**
 * Анализ 5: FALLBACK
 */
int getFallbackPrediction() {
    return (history.length % 3) + 1;
}

/**
 * ГЛАВНАЯ ФУНКЦИЯ ПРОГНОЗА — УСИЛЕННАЯ ИЕРАРХИЯ
 */
int getPrediction() {
    int pred;
    if ((pred = analyzeInteractionPatterns(MAX_PATTERN_LENGTH)) != -1) return pred;
    if ((pred = analyzeOutcomeReaction()) != -1) return pred;
    if ((pred = analyzePatterns(MAX_PATTERN_LENGTH)) != -1) return pred;
    if ((pred = predictLastMove()) != -1) return pred;  // НОВЫЙ УРОВЕНЬ
    if ((pred = analyzeFrequency()) != -1) return pred;
    return getFallbackPrediction();
}

/* ============= АДАПТАЦИЯ И СТРАТЕГИИ ============= */

void switchStrategyIfNeeded(int outcome) {
    if (outcome == LOSE) {
        current_lose_streak++;
        if (current_lose_streak >= LOSE_STREAK_THRESHOLD) {
            current_strategy = (current_strategy == 1) ? 2 : 1;
            current_lose_streak = 0;
        }
    } else {
        current_lose_streak = 0;
    }
}

int cycleMove(int move) {
    if (move == ROCK) return PAPER;
    if (move == PAPER) return SCISSORS;
    if (move == SCISSORS) return ROCK;
    return PAPER;
}

/* ============= ГЛАВНАЯ ЛОГИКА ============= */

int choose(int previousOpponentChoice) {
    current_round++;

    if (previousOpponentChoice == 0) {
        history.ourMoves[0] = PAPER;
        history.opponentMoves[0] = 0;
        history.length = 1;
        current_strategy = 1;
        current_lose_streak = 0;
        return PAPER;
    }

    history.opponentMoves[history.length - 1] = previousOpponentChoice;
    int outcome = determineOutcome(history.ourMoves[history.length - 1], previousOpponentChoice);
    history.outcomes[history.length - 1] = outcome;

    int prediction = getPrediction();
    int ourMove;

    if (current_strategy == 1) {
        ourMove = getCounterMove(prediction);

        // АНТИ-СТАГНАЦИЯ: 2 поражения подряд → цикл
        if (history.length >= 2 &&
            history.outcomes[history.length-1] == LOSE &&
            history.outcomes[history.length-2] == LOSE) {
            ourMove = cycleMove(ourMove);
        }

        // АНТИ-ПОВТОР: 3 одинаковых хода + 1 поражение → цикл
        if (history.length >= 3 &&
            history.ourMoves[history.length-1] == history.ourMoves[history.length-2] &&
            history.ourMoves[history.length-2] == history.ourMoves[history.length-3] &&
            history.outcomes[history.length-1] == LOSE) {
            ourMove = cycleMove(ourMove);
        }

    } else {
        static int sequence[10] = {SCISSORS, SCISSORS, PAPER, ROCK, SCISSORS,
                                   ROCK, PAPER, SCISSORS, PAPER, PAPER};
        static int idx = 0;
        if (idx < 6) {
            ourMove = getCounterMove(prediction);
        } else if (idx < 8) {
            // ПРОВОКАЦИЯ: контр к своему предыдущему
            ourMove = beats(history.ourMoves[history.length-1]);
        } else {
            ourMove = sequence[idx];
        }
        idx = (idx + 1) % 10;
    }

    switchStrategyIfNeeded(outcome);

    if (history.length < MAX_HISTORY) {
        history.ourMoves[history.length] = ourMove;
        history.opponentMoves[history.length] = 0;
        history.length++;
    }

    return ourMove;
}

void onGameStart() {
    memset(&history, 0, sizeof(history));
    current_round = 0;
    current_strategy = 1;
    current_lose_streak = 0;
}

void setParameters(int setCount, int winsPerSet) {
    // Не используется
}