package com.polus.fibicomp.coi.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.coi.dto.CoiDisclosureDto;
import com.polus.fibicomp.coi.dto.DisclosureActionLogDto;
import com.polus.fibicomp.coi.service.ConflictOfInterestService;
import com.polus.fibicomp.dashboard.vo.CoiDashboardVO;

@RestController
@RequestMapping("/coi/disclosure")
public class DisclosureController {

    @Autowired
    private ConflictOfInterestService conflictOfInterestService;

    @PostMapping("/historyDashboard")
    public ResponseEntity<Object> getDisclosureHistory(@RequestBody CoiDashboardVO dashboardVO) {
        return conflictOfInterestService.getDisclosureHistory(dashboardVO);
    }

    @PutMapping("/modifyRisk")
    public ResponseEntity<Object> modifyRisk(@RequestBody CoiDisclosureDto disclosureDto) {
        return conflictOfInterestService.modifyDisclosureRisk(disclosureDto);
    }

    @GetMapping("/risk")
    public ResponseEntity<Object> fetchAllDisclosureRisk() {
        return conflictOfInterestService.fetchAllDisclosureRisk();
    }

    @PostMapping("/history")
    public ResponseEntity<Object> fetchDisclosureHistory(@RequestBody DisclosureActionLogDto actionLogDto) {
        return conflictOfInterestService.fetchDisclosureHistory(actionLogDto);
    }

    @PostMapping("/riskStatus")
    public ResponseEntity<Object> checkRiskStatus(@RequestBody CoiDisclosureDto disclosureDto) {
        return conflictOfInterestService.checkDisclosureRiskStatus(disclosureDto);
    }

    @GetMapping("/projects/{disclosureId}")
    public ResponseEntity<Object>getDisclosureProjects(@PathVariable("disclosureId") Integer disclosureId) {
        return conflictOfInterestService.getDisclosureProjects(disclosureId);
    }

    @GetMapping("/lookups")
    public ResponseEntity<Object>getDisclosureLookups() {
        return conflictOfInterestService.getDisclosureLookups();
    }
}
