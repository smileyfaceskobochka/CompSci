package com.vyatsu.racing;

import com.vyatsu.racing.controllers.RacingController;
import com.vyatsu.racing.models.RacingDriver;
import com.vyatsu.racing.models.RacingSeries;
import com.vyatsu.racing.models.RacingTeam;
import com.vyatsu.racing.services.RacingService;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;

import java.util.*;

import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@RunWith(SpringRunner.class)
@WebMvcTest(RacingController.class)
public class RacingControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private RacingService racingService;

    @Test
    public void testIndexPage() throws Exception {
        RacingSeries series = new RacingSeries();
        series.setId(1L);
        series.setName("F1");
        series.setHeadquarters("HQ");
        RacingTeam team = new RacingTeam();
        team.setId(1L);
        team.setName("Test Team");
        team.setPrincipalName("Principal");
        team.setBaseLocation("Base");
        team.setPoints(100);
        RacingDriver driver = new RacingDriver();
        driver.setId(1L);
        driver.setFirstName("Max");
        driver.setLastName("Test");
        driver.setRacingNumber(1);
        driver.setPoints(100);
        team.setDrivers(Collections.singletonList(driver));
        driver.setTeam(team);
        series.setTeams(Collections.singletonList(team));

        when(racingService.getAllTeams()).thenReturn(Collections.singletonList(team));
        when(racingService.getAllDrivers()).thenReturn(Collections.singletonList(driver));
        when(racingService.getAllSeriesWithTeams()).thenReturn(Collections.singletonList(series));
        LinkedHashMap<RacingSeries, List<RacingDriver>> standings = new LinkedHashMap<>();
        standings.put(series, Collections.singletonList(driver));
        when(racingService.getStandingsBySeries()).thenReturn(standings);

        mockMvc.perform(get("/"))
            .andExpect(status().isOk())
            .andExpect(view().name("index"));
    }

    @Test
    public void testTeamsPage() throws Exception {
        RacingSeries series = new RacingSeries();
        series.setId(1L);
        series.setName("F1");
        series.setHeadquarters("HQ");
        RacingTeam team = new RacingTeam();
        team.setId(1L);
        team.setName("Test Team");
        team.setPrincipalName("Principal");
        team.setBaseLocation("Base");
        team.setPoints(100);
        series.setTeams(Collections.singletonList(team));

        when(racingService.getAllSeriesWithTeams()).thenReturn(Collections.singletonList(series));

        mockMvc.perform(get("/teams"))
            .andExpect(status().isOk())
            .andExpect(view().name("teams"));
    }

    @Test
    public void testDriversPage() throws Exception {
        RacingSeries series = new RacingSeries();
        series.setId(1L);
        series.setName("F1");
        series.setHeadquarters("HQ");
        RacingTeam team = new RacingTeam();
        team.setId(1L);
        team.setName("Test Team");
        team.setPrincipalName("Principal");
        team.setBaseLocation("Base");
        team.setPoints(100);

        RacingDriver driver = new RacingDriver();
        driver.setId(1L);
        driver.setFirstName("Max");
        driver.setLastName("Test");
        driver.setRacingNumber(1);
        driver.setPoints(100);
        driver.setTeam(team);
        team.setDrivers(Collections.singletonList(driver));
        series.setTeams(Collections.singletonList(team));

        when(racingService.getAllSeriesWithTeams()).thenReturn(Collections.singletonList(series));
        LinkedHashMap<RacingSeries, List<RacingDriver>> standings = new LinkedHashMap<>();
        standings.put(series, Collections.singletonList(driver));
        when(racingService.getStandingsBySeries()).thenReturn(standings);

        mockMvc.perform(get("/drivers"))
            .andExpect(status().isOk())
            .andExpect(view().name("drivers"));
    }

    @Test
    public void testSeriesPage() throws Exception {
        mockMvc.perform(get("/series"))
            .andExpect(status().isOk())
            .andExpect(view().name("series"));
    }

    @Test
    public void testSeriesAddForm() throws Exception {
        mockMvc.perform(get("/series/add"))
            .andExpect(status().isOk())
            .andExpect(view().name("series-form"));
    }

    @Test
    public void testTeamAddForm() throws Exception {
        mockMvc.perform(get("/teams/add"))
            .andExpect(status().isOk())
            .andExpect(view().name("team-form"));
    }

    @Test
    public void testDriverAddForm() throws Exception {
        mockMvc.perform(get("/drivers/add"))
            .andExpect(status().isOk())
            .andExpect(view().name("driver-form"));
    }
}
