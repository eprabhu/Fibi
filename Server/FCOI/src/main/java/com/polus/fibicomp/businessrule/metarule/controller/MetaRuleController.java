package com.polus.fibicomp.businessrule.metarule.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.businessrule.metarule.service.MetaRuleService;
import com.polus.fibicomp.businessrule.metarule.vo.MetaRuleVO;

@RestController
public class MetaRuleController {

	protected static Logger logger = LogManager.getLogger(MetaRuleController.class.getName());

	@Autowired
	@Qualifier(value = "metaRuleService")
	private MetaRuleService metaRuleService;

	@PostMapping(value = "/fetchMetaRulesByParams")
	public String fetchMetaRulesByParams(@RequestBody MetaRuleVO metaRuleVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchMetaRulesByParams");
		return metaRuleService.fetchMetaRulesByParams(metaRuleVO);
	}

	@PostMapping(value = "/saveOrUpdateMetaRule")
	public String saveOrUpdateMetaRule(@RequestBody MetaRuleVO metaRuleVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for save or update meta rule");
		return metaRuleService.saveOrUpdateMetaRule(metaRuleVO);
	}

	@PostMapping(value = "/deleteMetaRuleNode")
	public String deleteMetaRuleNode(@RequestBody MetaRuleVO metaRuleVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for delete meta rule node");
		return metaRuleService.deleteMetaRuleNode(metaRuleVO);
	}

}
