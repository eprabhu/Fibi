package com.polus.fibicomp.orcid.comparator;

import java.util.Comparator;

import org.apache.commons.lang3.builder.CompareToBuilder;

import com.polus.fibicomp.orcid.pojo.PersonOrcidWork;

public class PersonOrcidWorkComparator implements Comparator<PersonOrcidWork>{

	@Override
	public int compare(PersonOrcidWork personOrcidWork1, PersonOrcidWork personOrcidWork2) {
		return new CompareToBuilder().append(personOrcidWork1.getOrcidWork().getOrcidWorkStatusCode(), personOrcidWork2.getOrcidWork().getOrcidWorkStatusCode())
				.append(personOrcidWork2.getOrcidWork().getOrcidWorkStatusCode(), personOrcidWork2.getOrcidWork().getOrcidWorkStatusCode()).toComparison();
	}

}
