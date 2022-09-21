package com.polus.fibicomp.orcid.dto;

import java.util.ArrayList;
import java.util.List;

import com.polus.fibicomp.orcid.pojo.AwardPersonOrcidWork;
import com.polus.fibicomp.orcid.pojo.OrcidWork;
import com.polus.fibicomp.orcid.pojo.OrcidWorkCategory;
import com.polus.fibicomp.orcid.pojo.OrcidWorkType;
import com.polus.fibicomp.orcid.pojo.PersonOrcidWork;
import com.polus.fibicomp.person.pojo.Person;

public class OrcidVO {

	private String orcidId;

	private List<String> orcidIds;

	private String personId;

	private List<OrcidWork> orcidWorks;

	private Person person;

	private Integer putCode;

	private String awardNumber;

	private Integer personOrcidWorkId;

	private AwardPersonOrcidWork awardPersonOrcidWork;

	private List<OrcidWorkType> orcidWorkTypes = new ArrayList<OrcidWorkType>();

	private List<OrcidWorkCategory> orcidWorkCategories = new ArrayList<OrcidWorkCategory>();

	private List<PersonOrcidWork> personOrcidWorks = new ArrayList<PersonOrcidWork>();

	private Integer awardPersonOrcidWorkId;

	private String message;

	private Integer awardId;

	private List<AwardPersonOrcidWork> awardPersonOrcidWorks = new ArrayList<AwardPersonOrcidWork>();

	private List<Integer> personOrcidWorkIds;

	private String updateUser;

	public String getOrcidId() {
		return orcidId;
	}

	public void setOrcidId(String orcidId) {
		this.orcidId = orcidId;
	}

	public List<String> getOrcidIds() {
		return orcidIds;
	}

	public void setOrcidIds(List<String> orcidIds) {
		this.orcidIds = orcidIds;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public List<OrcidWork> getOrcidWorks() {
		return orcidWorks;
	}

	public void setOrcidWorks(List<OrcidWork> orcidWorks) {
		this.orcidWorks = orcidWorks;
	}

	public Person getPerson() {
		return person;
	}

	public void setPerson(Person person) {
		this.person = person;
	}

	public Integer getPutCode() {
		return putCode;
	}

	public void setPutCode(Integer putCode) {
		this.putCode = putCode;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public Integer getPersonOrcidWorkId() {
		return personOrcidWorkId;
	}

	public void setPersonOrcidWorkId(Integer personOrcidWorkId) {
		this.personOrcidWorkId = personOrcidWorkId;
	}

	public AwardPersonOrcidWork getAwardPersonOrcidWork() {
		return awardPersonOrcidWork;
	}

	public void setAwardPersonOrcidWork(AwardPersonOrcidWork awardPersonOrcidWork) {
		this.awardPersonOrcidWork = awardPersonOrcidWork;
	}

	public List<OrcidWorkType> getOrcidWorkTypes() {
		return orcidWorkTypes;
	}

	public void setOrcidWorkTypes(List<OrcidWorkType> orcidWorkTypes) {
		this.orcidWorkTypes = orcidWorkTypes;
	}

	public List<OrcidWorkCategory> getOrcidWorkCategories() {
		return orcidWorkCategories;
	}

	public void setOrcidWorkCategories(List<OrcidWorkCategory> orcidWorkCategories) {
		this.orcidWorkCategories = orcidWorkCategories;
	}

	public List<PersonOrcidWork> getPersonOrcidWorks() {
		return personOrcidWorks;
	}

	public void setPersonOrcidWorks(List<PersonOrcidWork> personOrcidWorks) {
		this.personOrcidWorks = personOrcidWorks;
	}

	public Integer getAwardPersonOrcidWorkId() {
		return awardPersonOrcidWorkId;
	}

	public void setAwardPersonOrcidWorkId(Integer awardPersonOrcidWorkId) {
		this.awardPersonOrcidWorkId = awardPersonOrcidWorkId;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public List<AwardPersonOrcidWork> getAwardPersonOrcidWorks() {
		return awardPersonOrcidWorks;
	}

	public void setAwardPersonOrcidWorks(List<AwardPersonOrcidWork> awardPersonOrcidWorks) {
		this.awardPersonOrcidWorks = awardPersonOrcidWorks;
	}

	public List<Integer> getPersonOrcidWorkIds() {
		return personOrcidWorkIds;
	}

	public void setPersonOrcidWorkIds(List<Integer> personOrcidWorkIds) {
		this.personOrcidWorkIds = personOrcidWorkIds;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

}
