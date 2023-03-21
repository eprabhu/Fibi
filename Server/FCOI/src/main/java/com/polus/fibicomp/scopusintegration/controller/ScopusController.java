package com.polus.fibicomp.scopusintegration.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.scopusintegration.service.ScopusService;
import com.polus.fibicomp.scopusintegration.vo.ScopusVO;
import com.polus.fibicomp.vo.CommonVO;

@RestController
public class ScopusController {

	protected static Logger logger = LogManager.getLogger(ScopusController.class.getName());

	@Autowired
	@Qualifier(value = "scopusService")
	private ScopusService scopusService;

	@PostMapping(value = "/findScopus")
	public String findScopus(@RequestBody CommonVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for finding Scopus");
		return scopusService.findScopus(vo.getSearchString());
	}

	@PostMapping(value = "/saveAwardScopus")
	public String saveAwardScopus(@RequestBody ScopusVO vo, HttpServletRequest request) {
		logger.info("Request for save AwardScopus");
		return scopusService.saveAwardScopus(vo);
	}

	@PostMapping(value = "/deleteAwardScopus")
	public String deleteAwardScopus(@RequestBody ScopusVO vo, HttpServletRequest request) {
		logger.info("Requesting for delete Scopus");
		return scopusService.deleteAwardScopus(vo);
	}

	@PostMapping(value = "/loadAllAwardScopus")
	public String loadAllAwardScopus(@RequestBody ScopusVO vo, HttpServletRequest request) {
		logger.info("Request for load all AwardScopus");
		return scopusService.loadAllAwardScopus(vo);
	}

	@GetMapping(value = "/scopusIntegration")
	public void scopusIntegration() {
		logger.info("Request for Scopus Integration API");
		scopusService.fetchScopusAPIResponse();
	}
}
