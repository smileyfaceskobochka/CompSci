package com.vyatsu.racing.models;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Table(name = "f2_teams")
public class F2Team extends RacingTeam {

    @Column(name = "chassis_model")
    private String chassisModel;

    @Column(nullable = false)
    private Integer graduates = 0;

    @Column(name = "is_feeder")
    private Boolean isFeeder = false;

    public String getChassisModel() { return chassisModel; }
    public void setChassisModel(String chassisModel) { this.chassisModel = chassisModel; }

    public Integer getGraduates() { return graduates; }
    public void setGraduates(Integer graduates) { this.graduates = graduates; }

    public Boolean getIsFeeder() { return isFeeder; }
    public void setIsFeeder(Boolean isFeeder) { this.isFeeder = isFeeder; }

    @Override
    public int calculatePoints(int position) {
        if (position < 1 || position > 10) return 0;
        int[] points = { 15, 12, 10, 8, 6, 5, 4, 3, 2, 1 };
        return points[position - 1];
    }
}
