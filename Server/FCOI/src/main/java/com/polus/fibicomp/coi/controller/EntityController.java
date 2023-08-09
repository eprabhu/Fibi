package com.polus.fibicomp.coi.controller;

import com.polus.fibicomp.coi.dto.CoiEntityDto;
import com.polus.fibicomp.coi.service.ActionLogService;
import com.polus.fibicomp.coi.service.ConflictOfInterestService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/coi")
public class EntityController {

    @Autowired
    private ActionLogService actionLogService;

    @Autowired
    private ConflictOfInterestService conflictOfInterestService;

    @PostMapping("/entity/modifyRisk")
    public ResponseEntity<Object> modifyRisk(@RequestBody CoiEntityDto entityDto) {
        return conflictOfInterestService.modifyRisk(entityDto);
    }

    @GetMapping("/entity/riskHistory/{entityId}")
    public ResponseEntity<Object> fetchEntityRiskHistory(@PathVariable("entityId") Integer entityId) {
        return conflictOfInterestService.fetchEntityRiskHistory(entityId);
    }

    @PostMapping("/entity/history")
    public ResponseEntity<Object> fetchEntityHistory(@RequestBody CoiEntityDto coiEntityDto) {
        return conflictOfInterestService.fetchEntityHistory(coiEntityDto);
    }
}
