package com.polus.fibicomp.orcid.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.polus.fibicomp.orcid.dto.OrcidWorkLinkedAwardDto;

@Entity
@Table(name = "PERSON_ORCID_WORK")
public class PersonOrcidWork implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PERSON_ORCID_WORK_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "PERSON_ORCID_WORK_ID_GENERATOR")
	@SequenceGenerator(name="PERSON_ORCID_WORK_ID_GENERATOR", sequenceName = "PERSON_ORCID_WORK_ID_GENERATOR", allocationSize=1)
	private Integer personOrcidWorkId;

	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "ORCID_ID")
	private String orcidId;

	@Column(name = "PUT_CODE")
	private Integer putCode;

	@OneToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "PERSON_ORCID_WORK_FK1"), name = "PUT_CODE", referencedColumnName = "PUT_CODE", insertable = false, updatable = false)
	private OrcidWork orcidWork;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Transient
	private List<OrcidWorkLinkedAwardDto> linkedAwards;

	@Transient
	private String personFullname;

	public PersonOrcidWork () {
		linkedAwards = new ArrayList<OrcidWorkLinkedAwardDto>();
	}

	public Integer getPersonOrcidWorkId() {
		return personOrcidWorkId;
	}

	public void setPersonOrcidWorkId(Integer personOrcidWorkId) {
		this.personOrcidWorkId = personOrcidWorkId;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getOrcidId() {
		return orcidId;
	}

	public void setOrcidId(String orcidId) {
		this.orcidId = orcidId;
	}

	public OrcidWork getOrcidWork() {
		return orcidWork;
	}

	public void setOrcidWork(OrcidWork orcidWork) {
		this.orcidWork = orcidWork;
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

	public Integer getPutCode() {
		return putCode;
	}

	public void setPutCode(Integer putCode) {
		this.putCode = putCode;
	}

	public List<OrcidWorkLinkedAwardDto> getLinkedAwards() {
		return linkedAwards;
	}

	public void setLinkedAwards(List<OrcidWorkLinkedAwardDto> linkedAwards) {
		this.linkedAwards = linkedAwards;
	}

	public String getPersonFullname() {
		return personFullname;
	}

	public void setPersonFullname(String personFullname) {
		this.personFullname = personFullname;
	}

}
