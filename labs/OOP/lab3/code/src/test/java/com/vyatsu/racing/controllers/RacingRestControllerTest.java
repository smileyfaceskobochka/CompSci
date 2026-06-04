package com.vyatsu.racing.controllers;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.vyatsu.racing.models.*;
import com.vyatsu.racing.repositories.*;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;

import java.util.*;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@RunWith(SpringRunner.class)
@WebMvcTest(RacingRestController.class)
public class RacingRestControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private RacingTeamRepository teamRepository;

    @MockBean
    private RacingDriverRepository driverRepository;

    @MockBean
    private RacingSeriesRepository seriesRepository;

    @Autowired
    private ObjectMapper objectMapper;

    private RacingSeries series;
    private RacingTeam team;
    private RacingDriver driver;

    @Before
    public void setUp() {
        series = new RacingSeries();
        series.setId(1L);
        series.setName("F1");
        series.setHeadquarters("HQ");

        team = new RacingTeam();
        team.setId(1L);
        team.setName("Test Team");
        team.setPrincipalName("Principal");
        team.setBaseLocation("Base");
        team.setPoints(100);
        team.setSeries(series);
        team.setWins(5);
        team.setPodiums(10);
        team.setPowerUnit("Mercedes");
        team.setBudgetCap(135.0);
        team.setConstructorPos(2);

        driver = new RacingDriver();
        driver.setId(1L);
        driver.setFirstName("Max");
        driver.setLastName("Verstappen");
        driver.setRacingNumber(33);
        driver.setPoints(150);
        driver.setTeam(team);

        team.setDrivers(Collections.singletonList(driver));
        series.setTeams(Collections.singletonList(team));
    }

    @Test
    public void testGetStatus() throws Exception {
        mockMvc.perform(get("/api/status"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.lab").value("lab3"))
                .andExpect(jsonPath("$.name").value("Java (Spring Boot)"));
    }

    @Test
    public void testGetAllTeams() throws Exception {
        when(teamRepository.findAll()).thenReturn(Collections.singletonList(team));

        mockMvc.perform(get("/api/teams"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$[0].teamName").value("Test Team"))
                .andExpect(jsonPath("$[0].principalName").value("Principal"));
    }

    @Test
    public void testCreateTeam() throws Exception {
        FormulaTeamDto dto = new FormulaTeamDto();
        dto.teamName = "New Team";
        dto.principalName = "New Principal";
        dto.headquarters = "New HQ";
        dto.type = "f1";
        dto.championshipPoints = 10;

        when(seriesRepository.findById(1L)).thenReturn(Optional.of(series));
        when(teamRepository.save(any(RacingTeam.class))).thenAnswer(invocation -> {
            RacingTeam saved = invocation.getArgument(0);
            saved.setId(2L);
            return saved;
        });

        mockMvc.perform(post("/api/teams")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(dto)))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.id").value(2))
                .andExpect(jsonPath("$.teamName").value("New Team"));
    }

    @Test
    public void testUpdateTeam() throws Exception {
        FormulaTeamDto dto = new FormulaTeamDto();
        dto.teamName = "Updated Team";
        dto.principalName = "Updated Principal";
        dto.headquarters = "Updated HQ";
        dto.type = "f1";

        when(teamRepository.findById(1L)).thenReturn(Optional.of(team));
        when(seriesRepository.findById(1L)).thenReturn(Optional.of(series));
        when(teamRepository.save(any(RacingTeam.class))).thenReturn(team);

        mockMvc.perform(put("/api/teams/1")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(dto)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.teamName").value("Updated Team"));
    }

    @Test
    public void testDeleteTeam() throws Exception {
        when(teamRepository.findById(1L)).thenReturn(Optional.of(team));
        doNothing().when(teamRepository).delete(team);

        mockMvc.perform(delete("/api/teams/1"))
                .andExpect(status().isNoContent());
    }

    @Test
    public void testGetAllDrivers() throws Exception {
        when(driverRepository.findAll()).thenReturn(Collections.singletonList(driver));

        mockMvc.perform(get("/api/drivers"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$[0].firstName").value("Max"))
                .andExpect(jsonPath("$[0].lastName").value("Verstappen"));
    }

    @Test
    public void testCreateDriver() throws Exception {
        FormulaDriverDto dto = new FormulaDriverDto();
        dto.firstName = "Lewis";
        dto.lastName = "Hamilton";
        dto.racingNumber = 44;
        dto.teamId = 1L;
        dto.points = 120;

        when(teamRepository.findById(1L)).thenReturn(Optional.of(team));
        when(driverRepository.save(any(RacingDriver.class))).thenAnswer(invocation -> {
            RacingDriver saved = invocation.getArgument(0);
            saved.setId(2L);
            return saved;
        });

        mockMvc.perform(post("/api/drivers")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(dto)))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.id").value(2))
                .andExpect(jsonPath("$.firstName").value("Lewis"));
    }

    @Test
    public void testDeleteDriver() throws Exception {
        when(driverRepository.findById(1L)).thenReturn(Optional.of(driver));
        doNothing().when(driverRepository).delete(driver);

        mockMvc.perform(delete("/api/drivers/1"))
                .andExpect(status().isNoContent());
    }

    @Test
    public void testGetStats() throws Exception {
        when(teamRepository.findAll()).thenReturn(Collections.singletonList(team));
        when(driverRepository.findAll()).thenReturn(Collections.singletonList(driver));

        mockMvc.perform(get("/api/stats"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.registeredTeamsCount").value(1))
                .andExpect(jsonPath("$.activeDriversCount").value(1));
    }

    @Test
    public void testSimulateRace() throws Exception {
        when(teamRepository.findById(1L)).thenReturn(Optional.of(team));
        when(driverRepository.findById(1L)).thenReturn(Optional.of(driver));
        when(driverRepository.save(any(RacingDriver.class))).thenReturn(driver);
        when(teamRepository.save(any(RacingTeam.class))).thenReturn(team);

        mockMvc.perform(post("/api/teams/1/simulate-race")
                        .param("position", "1")
                        .param("driverId", "1"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message").exists())
                .andExpect(jsonPath("$.team").exists());
    }

    @Test
    public void testCompareTeams() throws Exception {
        RacingTeam teamB = new RacingTeam();
        teamB.setId(2L);
        teamB.setName("Team B");
        teamB.setPoints(80);

        when(teamRepository.findById(1L)).thenReturn(Optional.of(team));
        when(teamRepository.findById(2L)).thenReturn(Optional.of(teamB));

        mockMvc.perform(get("/api/teams/compare")
                        .param("teamIdA", "1")
                        .param("teamIdB", "2"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.winnerId").value(1))
                .andExpect(jsonPath("$.message").value("Test Team is ahead of Team B in the championship."));
    }
}
