package com.polus.fibicomp.coi.controller;

import com.polus.fibicomp.coi.vo.CoiDashboardVO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.coi.dto.CoiDisclosureDto;
import com.polus.fibicomp.coi.dto.DisclosureActionLogDto;
import com.polus.fibicomp.coi.service.ConflictOfInterestService;

@RestController
@RequestMapping("/coi")
public class DisclosureController {

    @Autowired
    private ConflictOfInterestService conflictOfInterestService;

    @PostMapping("/disclosure/historyDashboard")
    public ResponseEntity<Object> getDisclosureHistory(@RequestBody CoiDashboardVO dashboardVO) {
        return conflictOfInterestService.getDisclosureHistory(dashboardVO);
    }

    @PutMapping("/disclosure/modifyRisk")
    public ResponseEntity<Object> modifyRisk(@RequestBody CoiDisclosureDto disclosureDto) {
        return conflictOfInterestService.modifyDisclosureRisk(disclosureDto);
    }

    @GetMapping("/disclosure/risk")
    public ResponseEntity<Object> fetchAllDisclosureRisk() {
        return conflictOfInterestService.fetchAllDisclosureRisk();
    }

    @PostMapping("/disclosure/history")
    public ResponseEntity<Object> fetchDisclosureHistory(@RequestBody DisclosureActionLogDto actionLogDto) {
        return conflictOfInterestService.fetchDisclosureHistory(actionLogDto);
    }


}
