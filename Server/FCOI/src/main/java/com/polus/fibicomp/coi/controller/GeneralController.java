package com.polus.fibicomp.coi.controller;

import com.polus.fibicomp.coi.service.GeneralService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * General Controller
 * Api other than modular functionalities
 * @created on 17-05-2023
 * @updated on 18-05-2023
 */
@RestController
public class GeneralController {

    @Autowired
    private GeneralService generalService;

    @GetMapping("/fetchAllCoiRights")
    public ResponseEntity<Object> fetchAllCoiRights(){
        return generalService.fetchAllCoiOpaRights();
    }
}
