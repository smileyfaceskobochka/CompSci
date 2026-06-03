package com.vyatsu.racing.models;

import java.util.List;

public class FormulaTeamDto {
    public Long id;
    public String type; // "f1", "f2", "fe"
    public String teamName;
    public String principalName;
    public String headquarters;
    public String team_color;
    public Integer championshipPoints;
    public Integer raceWins;
    public Integer podiums;
    public List<FormulaDriverDto> drivers;
    
    // UI properties
    public String seriesName;
    public String summary;
    public String visible_color;
    
    // F1 properties
    public String powerUnit;
    public Double budgetCapMln;
    public Integer constructorPos;
    public String engine_status;
    
    // F2 properties
    public String chassisModel;
    public Integer f1Graduates;
    public Boolean isFeederSeries;
    public String feeder_status_text;
    
    // FE properties
    public String energyPartner;
    public Double batteryCapacityKwh;
    public Integer sustainabilityScore;
    public String battery_rating_text;
}
