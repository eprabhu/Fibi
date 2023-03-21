package com.polus.fibicomp.proposal.pojo;

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

import com.polus.fibicomp.person.pojo.DegreeType;

@Entity
@Table(name = "EPS_PROPOSAL_PERSON_DEGREE")
public class ProposalPersonDegree implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PROPOSAL_PERSON_DEGREE_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer proposalPersonDegreeId;

	@Column(name = "PROPOSAL_PERSON_ID")
	private Integer proposalPersonId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROPOSAL_PERSONS_FK3"), name = "PROPOSAL_PERSON_ID", referencedColumnName = "PROPOSAL_PERSON_ID", insertable = false, updatable = false)
	private ProposalPerson proposalPerson;

	@Column(name = "DEGREE_CODE")
	private String degreeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "DEGREE_CODE_FK3"), name = "DEGREE_CODE", referencedColumnName = "DEGREE_CODE", insertable = false, updatable = false)
	private DegreeType degreeType;

	@Column(name = "DEGREE")
	private String degree;

	@Column(name = "FIELD_OF_STUDY")
	private String fieldOfStudy;

	@Column(name = "SPECIALIZATION")
	private String specialization;

	@Column(name = "SCHOOL")
	private String school;

	@Column(name = "GRADUATION_DATE")
	private String graduationDate;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getProposalPersonDegreeId() {
		return proposalPersonDegreeId;
	}

	public void setProposalPersonDegreeId(Integer proposalPersonDegreeId) {
		this.proposalPersonDegreeId = proposalPersonDegreeId;
	}

	public Integer getProposalPersonId() {
		return proposalPersonId;
	}

	public void setProposalPersonId(Integer proposalPersonId) {
		this.proposalPersonId = proposalPersonId;
	}

	public ProposalPerson getProposalPerson() {
		return proposalPerson;
	}

	public void setProposalPerson(ProposalPerson proposalPerson) {
		this.proposalPerson = proposalPerson;
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
