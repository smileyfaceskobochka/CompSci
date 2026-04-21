package com.vyatsu.racing.models;

import lombok.Data;
import lombok.ToString;
import javax.persistence.*;
import java.util.List;

@Entity
@Table(name = "racing_teams")
public class RacingTeam {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private String name;

    @Column(name = "principal_name", nullable = false)
    private String principalName;

    @Column(name = "base_location", nullable = false)
    private String baseLocation;

    @Column(name = "power_unit")
    private String powerUnit;

    @Column(nullable = false)
    private Integer points = 0;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "series_id", nullable = false)
    @ToString.Exclude
    private RacingSeries series;

    @OneToMany(mappedBy = "team", cascade = CascadeType.ALL)
    @ToString.Exclude
    private List<RacingDriver> drivers;

    @Column(name = "team_color")
    private String teamColor = "#FFFFFF";

    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }

    public String getName() { return name; }
    public void setName(String name) { this.name = name; }

    public String getPrincipalName() { return principalName; }
    public void setPrincipalName(String principalName) { this.principalName = principalName; }

    public String getBaseLocation() { return baseLocation; }
    public void setBaseLocation(String baseLocation) { this.baseLocation = baseLocation; }

    public String getPowerUnit() { return powerUnit; }
    public void setPowerUnit(String powerUnit) { this.powerUnit = powerUnit; }

    public Integer getPoints() { return points; }
    public void setPoints(Integer points) { this.points = points; }

    public String getTeamColor() { return teamColor; }
    public void setTeamColor(String teamColor) { this.teamColor = teamColor; }

    public RacingSeries getSeries() { return series; }
    public void setSeries(RacingSeries series) { this.series = series; }

    public List<RacingDriver> getDrivers() { return drivers; }
    public void setDrivers(List<RacingDriver> drivers) { this.drivers = drivers; }
}
