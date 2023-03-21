package com.polus.fibicomp.proposal.lookup.service;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.budget.pojo.BudgetCategory;
import com.polus.fibicomp.budget.pojo.CostElement;
import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.pojo.ScienceKeyword;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.proposal.dao.ProposalDao;
import com.polus.fibicomp.proposal.lookup.dao.ProposalLookUpDao;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.vo.SponsorSearchResult;

@Transactional
@Configuration
@Service(value = "proposalLookUpService")
public class ProposalLookUpServiceImpl implements ProposalLookUpService {

	protected static Logger logger = LogManager.getLogger(ProposalLookUpServiceImpl.class.getName());

	@Autowired
	@Qualifier(value = "proposalLookUpDao")
	private ProposalLookUpDao proposalLookUpDao;

	@Autowired
	@Qualifier(value = "proposalDao")
	private ProposalDao proposalDao;

	@Autowired
	@Qualifier(value = "personDao")
	private PersonDao personDao;

	@Override
	public List<SponsorSearchResult> findSponsor(String searchString) {
		return proposalLookUpDao.findSponsor(searchString);
	}

	@Override
	public List<GrantCall> getGrantCallsBasedOnSearchString(String searchString, Integer moduleCode, Boolean includeClosedGrantCall) {
		return  proposalLookUpDao.getGrantCallsBasedOnSearchString(searchString, moduleCode, includeClosedGrantCall, AuthenticatedUser.getLoginPersonIsExternal());
	}

	@Override
	public List<Unit> getUnitNumberForDepartment(String searchString, String check, List<String> rightName,
			String personId) {
		List<Unit> units = proposalLookUpDao.getDepartmentList(searchString);
		List<Unit> unitHasRight = new ArrayList<Unit>();
		if (units != null && !units.isEmpty() && rightName != null && !rightName.isEmpty()) {
			if (check.equals("any")) {
				for (Unit unit : units) {
					String unitNumber = unit.getUnitNumber();
					for (String rights : rightName) {
						Boolean status = proposalDao.checkUnitNumberHasRight(personId, unitNumber, rights);
						if (status) {
							unitHasRight.add(unit);
							break;
						}
					}
				}
				return unitHasRight;
			} else if (check.equals("all")) {
				AtomicBoolean valueCheck = new AtomicBoolean(false);
				units.forEach(unit -> {
					rightName.forEach(rights -> {
						String unitNumber = unit.getUnitNumber();
						Boolean status = proposalDao.checkUnitNumberHasRight(personId, unitNumber, rights);
						if (!status) {
							valueCheck.set(false);
							return;
						} else {
							valueCheck.set(true);
						}
					});
					if (valueCheck.get()) {
						unitHasRight.add(unit);
					}
				});
				return unitHasRight;
			}
		}
		return units;
	}

	@Override
	public List<CostElement> findCostElementByParams(String searchString, List<String> budgetCategoryCodes) {
		return proposalLookUpDao.findCostElementByParams(searchString, budgetCategoryCodes);
	}

	@Override
	public List<ScienceKeyword> findKeyWordsList(String searchString) {
		return proposalLookUpDao.findKeyWordsList(searchString);
	}

	@Override
	public List<Unit> findLeadUnitsList(String searchString) {
		return proposalLookUpDao.getDepartmentList(searchString);
	}

	@Override
	public List<BudgetCategory> findBudgetCategoryList(String searchString) {
		return proposalLookUpDao.findBudgetCategoryList(searchString);
	}

}
