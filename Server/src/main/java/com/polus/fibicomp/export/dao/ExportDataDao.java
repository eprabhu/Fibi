package com.polus.fibicomp.export.dao;

import java.util.List;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardFundingProposal;
import com.polus.fibicomp.ip.pojo.InstituteProposalAdminDetail;

@Service
public interface ExportDataDao {

	public List<Award> getAllActiAwards();

	public List<AwardFundingProposal> getAwardFundingProposal(Integer awardId);

	public InstituteProposalAdminDetail getInstituteProposalAdminDetail(Integer proposalId);
}
