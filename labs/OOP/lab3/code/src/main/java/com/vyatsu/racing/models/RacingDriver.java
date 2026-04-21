package com.vyatsu.racing.models;

import lombok.Data;
import lombok.ToString;
import javax.persistence.*;

@Entity
@Table(name = "racing_drivers")
public class RacingDriver {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "first_name", nullable = false)
    private String firstName;

    @Column(name = "last_name", nullable = false)
    private String lastName;

    @Column(name = "racing_number", nullable = false)
    private Integer racingNumber;

    @Column(nullable = false)
    private Integer points = 0;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "team_id", nullable = false)
    @ToString.Exclude
    private RacingTeam team;

    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }

    public String getFirstName() { return firstName; }
    public void setFirstName(String firstName) { this.firstName = firstName; }

    public String getLastName() { return lastName; }
    public void setLastName(String lastName) { this.lastName = lastName; }

    public Integer getRacingNumber() { return racingNumber; }
    public void setRacingNumber(Integer racingNumber) { this.racingNumber = racingNumber; }

    public Integer getPoints() { return points; }
    public void setPoints(Integer points) { this.points = points; }

    public RacingTeam getTeam() { return team; }
    public void setTeam(RacingTeam team) { this.team = team; }
}
