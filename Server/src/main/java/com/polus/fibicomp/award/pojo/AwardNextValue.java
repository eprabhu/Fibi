package com.polus.fibicomp.award.pojo;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "AWARD_NEXTVALUE")
public class AwardNextValue implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_NUMBER")
	private Integer awardNumber;

	public Integer getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(Integer awardNumber) {
		this.awardNumber = awardNumber;
	}	
	
}
