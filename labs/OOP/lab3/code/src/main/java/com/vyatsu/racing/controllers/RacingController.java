package com.vyatsu.racing.controllers;

import com.vyatsu.racing.models.RacingDriver;
import com.vyatsu.racing.models.RacingSeries;
import com.vyatsu.racing.models.RacingTeam;
import com.vyatsu.racing.services.RacingService;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

@Controller
public class RacingController {

    private final RacingService racingService;

    public RacingController(RacingService racingService) {
        this.racingService = racingService;
    }

    @GetMapping("/")
    public String index(Model model) {
        model.addAttribute("teams", racingService.getAllTeams());
        model.addAttribute("drivers", racingService.getAllDrivers());
        model.addAttribute("seriesList", racingService.getAllSeriesWithTeams());
        model.addAttribute("standingsBySeries", racingService.getStandingsBySeries());
        return "index";
    }

    @GetMapping("/series")
    public String viewSeries(Model model) {
        model.addAttribute("seriesList", racingService.getAllSeriesWithTeams());
        return "series";
    }

    @GetMapping("/series/add")
    public String showAddSeriesForm(Model model) {
        model.addAttribute("series", new RacingSeries());
        return "series-form";
    }

    @GetMapping("/series/edit/{id}")
    public String showEditSeriesForm(@PathVariable Long id, Model model) {
        model.addAttribute("series", racingService.getSeriesById(id));
        return "series-form";
    }

    @PostMapping("/series/save")
    public String saveSeries(@ModelAttribute RacingSeries series) {
        racingService.saveSeries(series);
        return "redirect:/series";
    }

    @GetMapping("/series/delete/{id}")
    public String deleteSeries(@PathVariable Long id) {
        racingService.deleteSeries(id);
        return "redirect:/series";
    }

    @GetMapping("/teams")
    public String viewTeams(Model model) {
        model.addAttribute("seriesList", racingService.getAllSeriesWithTeams());
        return "teams";
    }

    @GetMapping("/teams/add")
    public String showAddTeamForm(Model model) {
        model.addAttribute("team", new RacingTeam());
        model.addAttribute("seriesList", racingService.getAllSeries());
        return "team-form";
    }

    @GetMapping("/teams/edit/{id}")
    public String showEditTeamForm(@PathVariable Long id, Model model) {
        model.addAttribute("team", racingService.getTeamById(id));
        model.addAttribute("seriesList", racingService.getAllSeries());
        return "team-form";
    }

    @PostMapping("/teams/save")
    public String saveTeam(@ModelAttribute RacingTeam team) {
        racingService.saveTeam(team);
        return "redirect:/teams";
    }

    @GetMapping("/teams/delete/{id}")
    public String deleteTeam(@PathVariable Long id) {
        racingService.deleteTeam(id);
        return "redirect:/teams";
    }

    @GetMapping("/drivers")
    public String viewDrivers(Model model) {
        model.addAttribute("seriesList", racingService.getAllSeriesWithTeams());
        model.addAttribute("standingsBySeries", racingService.getStandingsBySeries());
        return "drivers";
    }

    @GetMapping("/drivers/add")
    public String showAddDriverForm(Model model) {
        model.addAttribute("driver", new RacingDriver());
        model.addAttribute("teams", racingService.getAllTeams());
        return "driver-form";
    }

    @GetMapping("/drivers/edit/{id}")
    public String showEditDriverForm(@PathVariable Long id, Model model) {
        model.addAttribute("driver", racingService.getDriverById(id));
        model.addAttribute("teams", racingService.getAllTeams());
        return "driver-form";
    }

    @PostMapping("/drivers/save")
    public String saveDriver(@ModelAttribute RacingDriver driver) {
        racingService.saveDriver(driver);
        return "redirect:/drivers";
    }

    @GetMapping("/drivers/delete/{id}")
    public String deleteDriver(@PathVariable Long id) {
        racingService.deleteDriver(id);
        return "redirect:/drivers";
    }
}
