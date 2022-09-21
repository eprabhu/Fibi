package com.polus.fibicomp.person.vo;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.person.pojo.PersonDegree;
import com.polus.fibicomp.persontraining.pojo.PersonTraining;
import com.polus.fibicomp.persontraining.pojo.PersonTrainingAttachment;
import com.polus.fibicomp.persontraining.pojo.PersonTrainingComment;

public class PersonVO {
	
	private Person person;
	
	private String personId;
	
	private String acType;
	
	private String message;
	
	private List<Person> persons;
	
	private String sortBy;
	
	private String reverse;
	
	private Integer currentPage;
	
	private Integer pageNumber;
	
	private Integer totalPersons;
	
	private String property1;
	
	private String property2;

	private String property3;

	private String property4;

	private String property5;
	
	private String property6;

	private String property7;

	private String property8;

	private String property9;

	private String property10;
	
	private String property11;
	
	private List<String> property12 = new ArrayList<>();
	
	private String documentHeading;
	
	private String exportType;
	
	private List<PersonTraining> trainings;
	
	private Map<String, String> sort = new HashMap<>();

	private String userName;
	
	private PersonTraining personTraining;
	
	private Integer trainingCode;
	
	private PersonTrainingComment personTrainingComment;
	
	private PersonTrainingAttachment personTrainingAttachment;
	
	private String searchString;
	
	private Integer totalResult;
	
	private List<PersonDegree> personDegrees;

	private PersonDegree personDegree;

	private List<PersonTrainingAttachment> personTrainingAttachments;

	public Person getPerson() {
		return person;
	}

	public void setPerson(Person person) {
		this.person = person;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public List<Person> getPersons() {
		return persons;
	}

	public void setPersons(List<Person> persons) {
		this.persons = persons;
	}

	public String getSortBy() {
		return sortBy;
	}

	public void setSortBy(String sortBy) {
		this.sortBy = sortBy;
	}

	public String getReverse() {
		return reverse;
	}

	public void setReverse(String reverse) {
		this.reverse = reverse;
	}

	public Integer getCurrentPage() {
		return currentPage;
	}

	public void setCurrentPage(Integer currentPage) {
		this.currentPage = currentPage;
	}

	public Integer getPageNumber() {
		return pageNumber;
	}

	public void setPageNumber(Integer pageNumber) {
		this.pageNumber = pageNumber;
	}

	public Integer getTotalPersons() {
		return totalPersons;
	}

	public void setTotalPersons(Integer totalPersons) {
		this.totalPersons = totalPersons;
	}

	public String getProperty1() {
		return property1;
	}

	public void setProperty1(String property1) {
		this.property1 = property1;
	}

	public String getProperty2() {
		return property2;
	}

	public void setProperty2(String property2) {
		this.property2 = property2;
	}

	public String getProperty3() {
		return property3;
	}

	public void setProperty3(String property3) {
		this.property3 = property3;
	}

	public String getProperty4() {
		return property4;
	}

	public void setProperty4(String property4) {
		this.property4 = property4;
	}

	public String getProperty5() {
		return property5;
	}

	public void setProperty5(String property5) {
		this.property5 = property5;
	}

	public String getProperty6() {
		return property6;
	}

	public void setProperty6(String property6) {
		this.property6 = property6;
	}

	public String getProperty7() {
		return property7;
	}

	public void setProperty7(String property7) {
		this.property7 = property7;
	}

	public String getProperty8() {
		return property8;
	}

	public void setProperty8(String property8) {
		this.property8 = property8;
	}

	public String getProperty9() {
		return property9;
	}

	public void setProperty9(String property9) {
		this.property9 = property9;
	}

	public String getProperty10() {
		return property10;
	}

	public void setProperty10(String property10) {
		this.property10 = property10;
	}
	
	public String getProperty11() {
		return property11;
	}

	public void setProperty11(String property11) {
		this.property11 = property11;
	}
	
	public List<String> getProperty12() {
		return property12;
	}

	public void setProperty12(List<String> property12) {
		this.property12 = property12;
	}

	public String getDocumentHeading() {
		return documentHeading;
	}

	public void setDocumentHeading(String documentHeading) {
		this.documentHeading = documentHeading;
	}

	public String getExportType() {
		return exportType;
	}

	public void setExportType(String exportType) {
		this.exportType = exportType;
	}

	public List<PersonTraining> getTrainings() {
		return trainings;
	}

	public void setTrainings(List<PersonTraining> trainings) {
		this.trainings = trainings;
	}

	public Map<String, String> getSort() {
		return sort;
	}

	public void setSort(Map<String, String> sort) {
		this.sort = sort;
	}

	public String getUserName() {
		return userName;
	}

	public void setUserName(String userName) {
		this.userName = userName;
	}

	public PersonTraining getPersonTraining() {
		return personTraining;
	}

	public void setPersonTraining(PersonTraining personTraining) {
		this.personTraining = personTraining;
	}

	public PersonTrainingComment getPersonTrainingComment() {
		return personTrainingComment;
	}

	public void setPersonTrainingComment(PersonTrainingComment personTrainingComment) {
		this.personTrainingComment = personTrainingComment;
	}

	public PersonTrainingAttachment getPersonTrainingAttachment() {
		return personTrainingAttachment;
	}

	public void setPersonTrainingAttachment(PersonTrainingAttachment personTrainingAttachment) {
		this.personTrainingAttachment = personTrainingAttachment;
	}

	public Integer getTrainingCode() {
		return trainingCode;
	}

	public void setTrainingCode(Integer trainingCode) {
		this.trainingCode = trainingCode;
	}

	public String getSearchString() {
		return searchString;
	}

	public void setSearchString(String searchString) {
		this.searchString = searchString;
	}

	public Integer getTotalResult() {
		return totalResult;
	}

	public void setTotalResult(Integer totalResult) {
		this.totalResult = totalResult;
	}

	public List<PersonTrainingAttachment> getPersonTrainingAttachments() {
		return personTrainingAttachments;
	}

	public void setPersonTrainingAttachments(List<PersonTrainingAttachment> personTrainingAttachments) {
		this.personTrainingAttachments = personTrainingAttachments;
	}

	public List<PersonDegree> getPersonDegrees() {
		return personDegrees;
	}

	public void setPersonDegrees(List<PersonDegree> personDegrees) {
		this.personDegrees = personDegrees;
	}

	public PersonDegree getPersonDegree() {
		return personDegree;
	}

	public void setPersonDegree(PersonDegree personDegree) {
		this.personDegree = personDegree;
	}

}
