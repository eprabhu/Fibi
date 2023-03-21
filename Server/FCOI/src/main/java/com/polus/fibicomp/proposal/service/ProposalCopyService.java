package com.polus.fibicomp.proposal.service;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.budget.pojo.BudgetHeader;
import com.polus.fibicomp.budget.pojo.BudgetPeriod;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.pojo.ProposalExtension;
import com.polus.fibicomp.proposal.vo.ProposalVO;

@Transactional
@Service(value = "proposalCopyService")
public interface ProposalCopyService {

	/**
	 * This method is used to make a copy of proposal.
	 * @param vo - Object of ProposalVO class.
	 * @return a String of details of proposal.
	 */
	public String copyProposal(ProposalVO vo);

	/**
	 * This method is used to make a copy Budget periods.
	 * @param copyBudget - Object of BudgetHeader class.
	 * @param originalBudget - Object of BudgetHeader class.
	 * @param activityTypeCode - activityTypeCode.
	 * @return a List of Budget Periods.
	 */
	public List<BudgetPeriod> copyBudgetPeriods(BudgetHeader copyBudget, BudgetHeader originalBudget, String activityTypeCode, String updateUser);

	public void copyProposalNonMandatoryFields(ProposalVO proposalVO, Proposal copyProposal, Proposal originalProposal, ProposalExtension copyProposalExtension, ProposalExtension originalProposalExtension);

}
