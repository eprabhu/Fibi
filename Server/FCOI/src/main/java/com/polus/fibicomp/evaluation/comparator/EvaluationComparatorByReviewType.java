package com.polus.fibicomp.evaluation.comparator;

import java.util.Comparator;

import org.apache.commons.lang3.builder.CompareToBuilder;

import com.polus.fibicomp.evaluation.pojo.ProposalReview;

public class EvaluationComparatorByReviewType implements Comparator<ProposalReview> {

	@Override
	public int compare(ProposalReview pr1, ProposalReview pr2) {
		return new CompareToBuilder().append(pr1.getReviewStartDate(), pr2.getReviewStartDate()).toComparison();
	}

}
