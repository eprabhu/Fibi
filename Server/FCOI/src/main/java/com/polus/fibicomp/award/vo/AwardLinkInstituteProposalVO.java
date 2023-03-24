package com.polus.fibicomp.award.vo;

import java.util.List;

import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardFundingProposal;
import com.polus.fibicomp.award.pojo.AwardMileStone;
import com.polus.fibicomp.ip.pojo.InstituteProposal;

public class AwardLinkInstituteProposalVO {

	private AwardFundingProposal awardFundingProposal;

	private List<AwardFundingProposal> awardFundingProposals;

	private InstituteProposal InstituteProposal;

	private Award award;

	private String acType;

	private String message;

	private Integer awardId;

	private Integer proposalId;

	private String updateUser;

	private List<String> ipNumbers;

	private boolean isDateofAwardRequired = false;

	private List<AwardMileStone> awardMileStones;

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public Award getAward() {
		return award;
	}

	public void setAward(Award award) {
		this.award = award;
	}

	public InstituteProposal getInstituteProposal() {
		return InstituteProposal;
	}

	public void setInstituteProposal(InstituteProposal instituteProposal) {
		InstituteProposal = instituteProposal;
	}

	public AwardFundingProposal getAwardFundingProposal() {
		return awardFundingProposal;
	}

	public void setAwardFundingProposal(AwardFundingProposal awardFundingProposal) {
		this.awardFundingProposal = awardFundingProposal;
	}

	public List<AwardFundingProposal> getAwardFundingProposals() {
		return awardFundingProposals;
	}

	public void setAwardFundingProposals(List<AwardFundingProposal> awardFundingProposals) {
		this.awardFundingProposals = awardFundingProposals;
	}

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public List<String> getIpNumbers() {
		return ipNumbers;
	}

	public void setIpNumbers(List<String> ipNumbers) {
		this.ipNumbers = ipNumbers;
	}

	public boolean isDateofAwardRequired() {
		return isDateofAwardRequired;
	}

	public void setDateofAwardRequired(boolean isDateofAwardRequired) {
		this.isDateofAwardRequired = isDateofAwardRequired;
	}

	public List<AwardMileStone> getAwardMileStones() {
		return awardMileStones;
	}

	public void setAwardMileStones(List<AwardMileStone> awardMileStones) {
		this.awardMileStones = awardMileStones;
	}
}
