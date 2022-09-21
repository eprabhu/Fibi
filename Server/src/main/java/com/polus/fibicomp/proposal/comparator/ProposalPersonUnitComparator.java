package com.polus.fibicomp.proposal.comparator;

import java.util.Comparator;

import org.apache.commons.lang3.builder.CompareToBuilder;

import com.polus.fibicomp.proposal.pojo.ProposalPersonUnit;

public class ProposalPersonUnitComparator implements Comparator<ProposalPersonUnit> {

	@Override
	public int compare(ProposalPersonUnit personUnitOne, ProposalPersonUnit personUnitTwo) {
		return new CompareToBuilder().append(personUnitTwo.isLeadUnit(), personUnitOne.isLeadUnit()).toComparison();
	}

}
