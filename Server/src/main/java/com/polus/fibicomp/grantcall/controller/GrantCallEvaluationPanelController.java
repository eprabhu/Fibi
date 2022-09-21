package com.polus.fibicomp.grantcall.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;
import com.polus.fibicomp.grantcall.service.GrantCallEvaluationPanelService;
import com.polus.fibicomp.grantcall.vo.GrantCallEvaluationPanelVO;

@RestController
public class GrantCallEvaluationPanelController {

	protected static Logger logger = LogManager.getLogger(GrantCallEvaluationPanelController.class.getName());

	@Autowired
	@Qualifier(value = "grantCallEvaluationService")
	private GrantCallEvaluationPanelService grantCallEvaluationService;

	@RequestMapping(value = "/fetchAllEvaluationPanels", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchAllEvaluationPanels(@RequestBody GrantCallEvaluationPanelVO vo,HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchAllEvaluationPanels");
		return grantCallEvaluationService.fetchAllEvaluationPanels(vo);
	}

	@RequestMapping(value = "/saveOrUpdateGrantCallEvaluationPanel", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateGrantCallEvaluationPanel(@RequestBody GrantCallEvaluationPanelVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveOrUpdateGrantCallEvaluationPanel");
		return grantCallEvaluationService.saveOrUpdateGrantCallEvaluationPanel(vo);
	}

	@RequestMapping(value = "/deleteEvaluationPanel", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteEvaluationPanel(@RequestBody GrantCallEvaluationPanelVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for delete EvaluationPanel");
		return grantCallEvaluationService.deleteEvaluationPanel(vo);
	}

}
