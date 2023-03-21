package com.polus.fibicomp.budget.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.service.AwardService;
import com.polus.fibicomp.budget.dao.AwardBudgetDao;
import com.polus.fibicomp.budget.dao.BudgetDao;
import com.polus.fibicomp.budget.pojo.BudgetPerson;
import com.polus.fibicomp.budget.vo.AwardBudgetVO;
import com.polus.fibicomp.budget.vo.BudgetVO;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.proposal.dao.ProposalDao;

@Transactional
@Service(value = "budgetPersonService")
public class BudgetPersonServiceImpl implements BudgetPersonService {

	@Autowired
	private BudgetDao budgetDao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private ProposalDao proposalDao;

	@Autowired
	private AwardBudgetDao awardBudgetDao;

	@Autowired
	private AwardDao awardDao;

	@Autowired
	private AwardService awardService;

	@Override
	public String getBudgetPersons(Integer budgetId, Integer proposalId) {
		BudgetVO budgetVO = new BudgetVO();
		budgetVO.setAppointmentType(budgetDao.loadAllAppointmentTypes());
		budgetVO.setTbnPersons(budgetDao.fetchAllTbnPerson());
		budgetVO.setJobCode(budgetDao.loadAllJobCodes());
		budgetVO.setBudgetPersonList(budgetDao.getBudgetPersons(budgetId));
		budgetVO.setProposalPersons(proposalDao.fetchProposalPersonBasedOnProposalId(proposalId));
		return commonDao.convertObjectToJSON(budgetVO);
	}

	@Override
	public String saveOrUpdateProposalBudgetPerson(BudgetPerson budgetPerson) {
		return commonDao.convertObjectToJSON(budgetDao.saveOrUpdateProposalBudgetPerson(budgetPerson));
	}

	@Override
	public String deleteBudgetPerson(Integer budgetPersonId, Integer budgetId) {
		BudgetVO budgetVO = new BudgetVO();
		budgetDao.deleteBudgetPerson(budgetPersonId);
		budgetVO.setBudgetPersonList(budgetDao.getBudgetPersons(budgetId));
		return commonDao.convertObjectToJSON(budgetVO);
	}

	@Override
	public Boolean checkBudgetPersonAddedInBudget(Integer budgetPersonId) {
		return budgetDao.checkBudgetPersonAddedInBudget(budgetPersonId);
	}

	@Override
	public String saveOrUpdateAwardBudgetPerson(AwardBudgetVO vo) {
		awardService.updateAwardDocumentUpdateUserAndTimestamp(vo.getAwardId(), vo.getAwardBudgetPerson().getUpdateUser());
		return commonDao.convertObjectToJSON(awardBudgetDao.saveOrUpdateAwardBudgetPerson(vo.getAwardBudgetPerson()));
	}

	@Override
	public String deleteAwardBudgetPerson(Integer budgetPersonId, Integer budgetId, Integer awardId, String userName) {
		AwardBudgetVO awardBudgetVO = new AwardBudgetVO();
		awardBudgetDao.deleteAwardBudgetPerson(budgetPersonId);
		awardBudgetVO.setAwardBudgetPersonList(awardBudgetDao.getBudgetPersons(budgetId));
		awardService.updateAwardDocumentUpdateUserAndTimestamp(awardId, userName);
		return commonDao.convertObjectToJSON(awardBudgetVO);
	}

	@Override
	public String getAwardBudgetPersons(Integer budgetHeaderId, String awardId) {
		AwardBudgetVO awardBudgetVO = new AwardBudgetVO();
		awardBudgetVO.setAppointmentType(budgetDao.loadAllAppointmentTypes());
		awardBudgetVO.setTbnPersons(budgetDao.fetchAllTbnPerson());
		awardBudgetVO.setJobCode(budgetDao.loadAllJobCodes());
		awardBudgetVO.setAwardBudgetPersonList(awardBudgetDao.getBudgetPersons(budgetHeaderId));
		awardBudgetVO.setAwardPersons(awardDao.fetchAllAwardPesonsByAwardId(awardId));
		return commonDao.convertObjectToJSON(awardBudgetVO);
	}

}
