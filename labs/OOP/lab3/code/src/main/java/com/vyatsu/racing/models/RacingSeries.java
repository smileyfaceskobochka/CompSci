package com.vyatsu.racing.models;

import javax.persistence.*;
import java.util.List;

@Entity
@Table(name = "racing_series")
public class RacingSeries {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private String name;

    @Column(name = "official_name")
    private String officialName;

    @Column(nullable = false)
    private String headquarters;

    @Column(name = "governing_body")
    private String governingBody;

    @Column(name = "founded_year")
    private Integer foundedYear;

    @Column(name = "rounds")
    private Integer rounds;

    @OneToMany(mappedBy = "series", cascade = CascadeType.ALL)
    private List<RacingTeam> teams;

    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }

    public String getName() { return name; }
    public void setName(String name) { this.name = name; }

    public String getOfficialName() { return officialName; }
    public void setOfficialName(String officialName) { this.officialName = officialName; }

    public String getHeadquarters() { return headquarters; }
    public void setHeadquarters(String headquarters) { this.headquarters = headquarters; }

    public String getGoverningBody() { return governingBody; }
    public void setGoverningBody(String governingBody) { this.governingBody = governingBody; }

    public Integer getFoundedYear() { return foundedYear; }
    public void setFoundedYear(Integer foundedYear) { this.foundedYear = foundedYear; }

    public Integer getRounds() { return rounds; }
    public void setRounds(Integer rounds) { this.rounds = rounds; }

    public List<RacingTeam> getTeams() { return teams; }
    public void setTeams(List<RacingTeam> teams) { this.teams = teams; }
}
