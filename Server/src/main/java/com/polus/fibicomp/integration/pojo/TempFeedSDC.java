package com.polus.fibicomp.integration.pojo;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "TMP_FEED_SDC_GM_FO_RA")
public class TempFeedSDC  implements Serializable{

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ID")
	private Integer id;

	@Column(name = "NAME")
	private String name;

	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "DEPT_CODE")
	private String deptCode;

	@Column(name = "DEPARTMENT_")
	private String department;

	@Column(name = "SDC")
	private String sdc;

	@Column(name = "GM")
	private String gm;

	@Column(name = "RESEARCH_OFFICE_ADMINISTRATOR_")
	private Integer reserchOfficeAdmin;

	@Column(name = "FIN_OFFICER")
	private String finOfficer;

	@Column(name = "PI_PERSON_ID")
	private String piPersonid;

	public Integer getId() {
		return id;
	}

	public void setId(Integer id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getDeptCode() {
		return deptCode;
	}

	public void setDeptCode(String deptCode) {
		this.deptCode = deptCode;
	}

	public String getDepartment() {
		return department;
	}

	public void setDepartment(String department) {
		this.department = department;
	}

	public String getSdc() {
		return sdc;
	}

	public void setSdc(String sdc) {
		this.sdc = sdc;
	}

	public String getGm() {
		return gm;
	}

	public void setGm(String gm) {
		this.gm = gm;
	}

	public Integer getReserchOfficeAdmin() {
		return reserchOfficeAdmin;
	}

	public void setReserchOfficeAdmin(Integer reserchOfficeAdmin) {
		this.reserchOfficeAdmin = reserchOfficeAdmin;
	}

	public String getFinOfficer() {
		return finOfficer;
	}

	public void setFinOfficer(String finOfficer) {
		this.finOfficer = finOfficer;
	}

	public String getPiPersonid() {
		return piPersonid;
	}

	public void setPiPersonid(String piPersonid) {
		this.piPersonid = piPersonid;
	}

}
