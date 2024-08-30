package com.polus.integration.award.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.polus.integration.award.service.AwardIntegrationService;
import com.polus.integration.award.vo.AwardIntegrationVO;

import lombok.extern.slf4j.Slf4j;

@RestController
@Slf4j
public class AwardIntegrationController {

	@Autowired
	private AwardIntegrationService awardService;

	@PostMapping("/feedAward")
	public void feedAward(@RequestBody AwardIntegrationVO vo) {
		log.info("Request for feedAward");
		awardService.feedAward(vo.getAward());
	}

}
