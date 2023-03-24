package com.polus.fibicomp.person.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Entity
@Table(name = "PERSON_DEGREE")
public class PersonDegree implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PERSON_DEGREE_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer personDegreeId;

	@Column(name = "PERSON_ID")
	private String personId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "PERSON_ID_FK4"), name = "PERSON_ID", referencedColumnName = "PERSON_ID", insertable = false, updatable = false)
	private Person person;

	@Column(name = "DEGREE_CODE")
	private String degreeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "DEGREE_CODE_FK4"), name = "DEGREE_CODE", referencedColumnName = "DEGREE_CODE", insertable = false, updatable = false)
	private DegreeType degreeType;

	@Column(name = "DEGREE")
	private String degree;

	@Column(name = "FIELD_OF_STUDY")
	private String fieldOfStudy;

	@Column(name = "SPECIALIZATION")
	private String specialization;

	@Column(name = "SCHOOL")
	private String school;

	@Column(name = "SCHOOL_ID_CODE")
	private String schoolIdCode;

	@Column(name = "SCHOOL_ID")
	private String schoolId;

	@Column(name = "GRADUATION_DATE")
	private String graduationDate;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getPersonDegreeId() {
		return personDegreeId;
	}

	public void setPersonDegreeId(Integer personDegreeId) {
		this.personDegreeId = personDegreeId;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getSchoolIdCode() {
		return schoolIdCode;
	}

	public void setSchoolIdCode(String schoolIdCode) {
		this.schoolIdCode = schoolIdCode;
	}

	public String getSchoolId() {
		return schoolId;
	}

	public void setSchoolId(String schoolId) {
		this.schoolId = schoolId;
	}

	public String getDegreeCode() {
		return degreeCode;
	}

	public void setDegreeCode(String degreeCode) {
		this.degreeCode = degreeCode;
	}

	public String getDegree() {
		return degree;
	}

	public void setDegree(String degree) {
		this.degree = degree;
	}

	public String getFieldOfStudy() {
		return fieldOfStudy;
	}

	public void setFieldOfStudy(String fieldOfStudy) {
		this.fieldOfStudy = fieldOfStudy;
	}

	public String getSpecialization() {
		return specialization;
	}

	public void setSpecialization(String specialization) {
		this.specialization = specialization;
	}

	public String getSchool() {
		return school;
	}

	public void setSchool(String school) {
		this.school = school;
	}

	public String getGraduationDate() {
		return graduationDate;
	}

	public void setGraduationDate(String graduationDate) {
		this.graduationDate = graduationDate;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public DegreeType getDegreeType() {
		return degreeType;
	}

	public void setDegreeType(DegreeType degreeType) {
		this.degreeType = degreeType;
	}

}
