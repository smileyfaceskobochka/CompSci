#include "FormulaDriver.h"

FormulaDriver::FormulaDriver(const std::string& fName, const std::string& lName, int number, int pts)
    : firstName(fName), lastName(lName), racingNumber(number), points(pts) {}

std::string FormulaDriver::getFirstName() const {
    return firstName;
}

std::string FormulaDriver::getLastName() const {
    return lastName;
}

std::string FormulaDriver::getFullName() const {
    return firstName + " " + lastName;
}

int FormulaDriver::getRacingNumber() const {
    return racingNumber;
}

int FormulaDriver::getPoints() const {
    return points;
}

void FormulaDriver::addPoints(int pts) {
    points += pts;
}
