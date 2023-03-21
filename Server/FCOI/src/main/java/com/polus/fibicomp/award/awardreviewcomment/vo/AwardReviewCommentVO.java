package com.polus.fibicomp.award.awardreviewcomment.vo;

import java.util.ArrayList;
import java.util.List;

import com.polus.fibicomp.award.awardreviewcomment.dto.AwardReviewCommentDto;
import com.polus.fibicomp.award.pojo.AwardReviewComment;
import com.polus.fibicomp.pojo.PersonDTO;

public class AwardReviewCommentVO {

	private Integer awardReviewCommentId;

	private AwardReviewComment awardReviewComment;

	private Integer awardId;

	private List<AwardReviewCommentDto> awardReviewComments;

	private String personId;

	private List<PersonDTO> persons = new ArrayList<>();

	private String message;

	private String loginPersonUnitNumber;

	private String awardNumber;

	public Integer getAwardReviewCommentId() {
		return awardReviewCommentId;
	}

	public void setAwardReviewCommentId(Integer awardReviewCommentId) {
		this.awardReviewCommentId = awardReviewCommentId;
	}

	public AwardReviewComment getAwardReviewComment() {
		return awardReviewComment;
	}

	public void setAwardReviewComment(AwardReviewComment awardReviewComment) {
		this.awardReviewComment = awardReviewComment;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public List<AwardReviewCommentDto> getAwardReviewComments() {
		return awardReviewComments;
	}

	public void setAwardReviewComments(List<AwardReviewCommentDto> awardReviewComments) {
		this.awardReviewComments = awardReviewComments;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public List<PersonDTO> getPersons() {
		return persons;
	}

	public void setPersons(List<PersonDTO> persons) {
		this.persons = persons;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public String getLoginPersonUnitNumber() {
		return loginPersonUnitNumber;
	}

	public void setLoginPersonUnitNumber(String loginPersonUnitNumber) {
		this.loginPersonUnitNumber = loginPersonUnitNumber;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

}
