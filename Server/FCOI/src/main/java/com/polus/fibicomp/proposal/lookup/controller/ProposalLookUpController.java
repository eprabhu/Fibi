package com.polus.fibicomp.proposal.lookup.controller;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.budget.pojo.BudgetCategory;
import com.polus.fibicomp.budget.pojo.CostElement;
import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.pojo.ScienceKeyword;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.proposal.lookup.service.ProposalLookUpService;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.vo.CommonVO;
import com.polus.fibicomp.vo.SponsorSearchResult;

@RestController
public class ProposalLookUpController {

	protected static Logger logger = LogManager.getLogger(ProposalLookUpController.class.getName());

	@Autowired
	@Qualifier(value = "proposalLookUpService")
	private ProposalLookUpService proposalLookUpService;

	private static final String SEARCH_STRING = "searchString : {}";

	@PostMapping(value = "/findSponsors")
	public List<SponsorSearchResult> getNext(@RequestBody CommonVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for findSponsors");
		logger.info(SEARCH_STRING, vo.getSearchString());
		return proposalLookUpService.findSponsor(vo.getSearchString());
	}

	@PostMapping(value = "/findGrantCall")
	public List<GrantCall> getGrantCall(@RequestBody CommonVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getGrantCall");
		logger.info(SEARCH_STRING, vo.getSearchString());
		return proposalLookUpService.getGrantCallsBasedOnSearchString(vo.getSearchString(), vo.getModuleCode(), vo.getIncludeClosedGrantCall());
	}

	@PostMapping(value = "/findLeadUnits")
	public List<Unit> findLeadUnits(@RequestBody CommonVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getDepartment");
		logger.info(SEARCH_STRING, vo.getSearchString());
		logger.info("Check: {}", vo.getCheck());
		logger.info("RightName : {}",vo.getRightName());
		logger.info("PersonId: {}", AuthenticatedUser.getLoginPersonId());
		return proposalLookUpService.getUnitNumberForDepartment(vo.getSearchString(), vo.getCheck(), vo.getRightName(), AuthenticatedUser.getLoginPersonId());
	}

	@PostMapping(value = "/findCostElement")
	public List<CostElement> findCostElement(@RequestBody CommonVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for findCostElement");
		logger.info(SEARCH_STRING, vo.getSearchString());
		return proposalLookUpService.findCostElementByParams(vo.getSearchString(), null);
	}

	@PostMapping(value = "/findKeyWords")
	public List<ScienceKeyword> findKeyWords(@RequestBody CommonVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for findKeyWords");
		logger.info(SEARCH_STRING, vo.getSearchString());
		return proposalLookUpService.findKeyWordsList(vo.getSearchString());
	}

	@PostMapping(value = "/findDepartment")
	public List<Unit> findDepartment(@RequestBody CommonVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for findDepartment");
		logger.info(SEARCH_STRING, vo.getSearchString());
		return proposalLookUpService.findLeadUnitsList(vo.getSearchString());
	}

	@PostMapping(value = "/findBudgetCategory")
	public List<BudgetCategory> findBudgetCategory(@RequestBody CommonVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getBudgetCategory");
		logger.info(SEARCH_STRING, vo.getSearchString());
		return proposalLookUpService.findBudgetCategoryList(vo.getSearchString());
	}

	@PostMapping(value = "/findCostElementsByParams")
	public List<CostElement> findCostElementsByParams(@RequestBody CommonVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for findCostElementsByParams");
		logger.info(SEARCH_STRING, vo.getSearchString());
		return proposalLookUpService.findCostElementByParams(vo.getSearchString(), vo.getBudgetCategoryCodes());
	}

}
