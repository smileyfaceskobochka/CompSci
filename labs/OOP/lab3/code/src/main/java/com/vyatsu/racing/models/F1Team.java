package com.vyatsu.racing.models;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Table(name = "f1_teams")
public class F1Team extends RacingTeam {

    @Column(name = "power_unit")
    private String powerUnit;

    @Column(name = "budget_cap")
    private Double budgetCap = 0.0;

    @Column(name = "constructor_pos")
    private Integer constructorPos = 0;

    public String getPowerUnit() { return powerUnit; }
    public void setPowerUnit(String powerUnit) { this.powerUnit = powerUnit; }

    public Double getBudgetCap() { return budgetCap; }
    public void setBudgetCap(Double budgetCap) { this.budgetCap = budgetCap; }

    public Integer getConstructorPos() { return constructorPos; }
    public void setConstructorPos(Integer constructorPos) { this.constructorPos = constructorPos; }

    @Override
    public int calculatePoints(int position) {
        if (position < 1 || position > 10) return 0;
        int[] points = { 25, 18, 15, 12, 10, 8, 6, 4, 2, 1 };
        return points[position - 1];
    }
}
