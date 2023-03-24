package com.polus.fibicomp.pojo;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "UNIT_WITH_CHILDREN")
public class UnitWithChildren implements Serializable{

		private static final long serialVersionUID = 1L;

		@Id
		@Column(name = "UNIT_NUMBER")
		private String unitNumber;

		@Column(name = "UNIT_NAME")
		private String unitName;

		@Column(name = "CHILD_UNIT_NUMBER")
		private String childUnitNumber;

		@Column(name = "CHILD_UNIT_NAME")
		private String childUnitName;

		public String getUnitNumber() {
			return unitNumber;
		}

		public void setUnitNumber(String unitNumber) {
			this.unitNumber = unitNumber;
		}

		public String getUnitName() {
			return unitName;
		}

		public void setUnitName(String unitName) {
			this.unitName = unitName;
		}

		public String getChildUniNumber() {
			return childUnitNumber;
		}

		public void setChildUniNumber(String childUniNumber) {
			this.childUnitNumber = childUniNumber;
		}

		public String getChildUnitName() {
			return childUnitName;
		}

		public void setChildUnitName(String childUnitName) {
			this.childUnitName = childUnitName;
		}

}
