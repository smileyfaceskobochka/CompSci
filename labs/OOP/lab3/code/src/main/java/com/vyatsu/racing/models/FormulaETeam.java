package com.vyatsu.racing.models;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Table(name = "formula_e_teams")
public class FormulaETeam extends RacingTeam {

    @Column(name = "energy_partner")
    private String energyPartner;

    @Column(name = "battery_kwh")
    private Double batteryKwh = 0.0;

    @Column(name = "sustain_score")
    private Integer sustainScore = 0;

    public String getEnergyPartner() { return energyPartner; }
    public void setEnergyPartner(String energyPartner) { this.energyPartner = energyPartner; }

    public Double getBatteryKwh() { return batteryKwh; }
    public void setBatteryKwh(Double batteryKwh) { this.batteryKwh = batteryKwh; }

    public Integer getSustainScore() { return sustainScore; }
    public void setSustainScore(Integer sustainScore) { this.sustainScore = sustainScore; }

    @Override
    public int calculatePoints(int position) {
        if (position < 1 || position > 10) return 0;
        int[] points = { 25, 18, 15, 12, 10, 8, 6, 4, 2, 1 };
        return points[position - 1];
    }
}
