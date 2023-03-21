//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.11 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2020.11.25 at 01:02:19 PM IST 
//


package com.polus.fibicomp.manpowerintegration.dto.recruiting;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * Wrapper element for the creation of a new position opening for a supervisory organization using position management.
 * 
 * <p>Java class for Create_Position_DataType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Create_Position_DataType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="Supervisory_Organization_Reference" type="{urn:com.workday/bsvc}Supervisory_OrganizationObjectType"/&gt;
 *         &lt;element name="Position_Request_Reason_Reference" type="{urn:com.workday/bsvc}Event_Classification_SubcategoryObjectType" minOccurs="0"/&gt;
 *         &lt;element name="Position_Data" type="{urn:com.workday/bsvc}Position_Definition_DataType"/&gt;
 *         &lt;element name="Qualification_Replacement_Data" type="{urn:com.workday/bsvc}Qualification_Data_for_Position_Restriction_or_Job_ProfileType" minOccurs="0"/&gt;
 *         &lt;element name="Position_Group_Restrictions_Data" type="{urn:com.workday/bsvc}Position_Group_Restriction_DataType" minOccurs="0"/&gt;
 *         &lt;element name="Edit_Assign_Organization_Sub_Process" type="{urn:com.workday/bsvc}Edit_Assign_Position_Organization_Sub_Business_ProcessType" minOccurs="0"/&gt;
 *         &lt;element name="Request_Default_Compensation_Sub_Process" type="{urn:com.workday/bsvc}Request_Compensation_Default_Sub_Business_ProcessType" minOccurs="0"/&gt;
 *         &lt;element name="Assign_Pay_Group_Sub_Process" type="{urn:com.workday/bsvc}Assign_Pay_Group_for_Position_Restrictions_Sub_Business_ProcessType" minOccurs="0"/&gt;
 *         &lt;element name="Assign_Costing_Allocation_Sub_Process" type="{urn:com.workday/bsvc}Assign_Costing_Allocation_Sub_Business_ProcessType" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Create_Position_DataType", propOrder = {
    "supervisoryOrganizationReference",
    "positionRequestReasonReference",
    "positionData",
    "qualificationReplacementData",
    "positionGroupRestrictionsData",
    "editAssignOrganizationSubProcess",
    "requestDefaultCompensationSubProcess",
    "assignPayGroupSubProcess",
    "assignCostingAllocationSubProcess"
})
public class CreatePositionDataType {

    @XmlElement(name = "Supervisory_Organization_Reference", required = true)
    protected SupervisoryOrganizationObjectType supervisoryOrganizationReference;
    @XmlElement(name = "Position_Request_Reason_Reference")
    protected EventClassificationSubcategoryObjectType positionRequestReasonReference;
    @XmlElement(name = "Position_Data", required = true)
    protected PositionDefinitionDataType positionData;
    @XmlElement(name = "Qualification_Replacement_Data")
    protected QualificationDataForPositionRestrictionOrJobProfileType qualificationReplacementData;
    @XmlElement(name = "Position_Group_Restrictions_Data")
    protected PositionGroupRestrictionDataType positionGroupRestrictionsData;
    @XmlElement(name = "Edit_Assign_Organization_Sub_Process")
    protected EditAssignPositionOrganizationSubBusinessProcessType editAssignOrganizationSubProcess;
    @XmlElement(name = "Request_Default_Compensation_Sub_Process")
    protected RequestCompensationDefaultSubBusinessProcessType requestDefaultCompensationSubProcess;
    @XmlElement(name = "Assign_Pay_Group_Sub_Process")
    protected AssignPayGroupForPositionRestrictionsSubBusinessProcessType assignPayGroupSubProcess;
    @XmlElement(name = "Assign_Costing_Allocation_Sub_Process")
    protected AssignCostingAllocationSubBusinessProcessType assignCostingAllocationSubProcess;

    /**
     * Gets the value of the supervisoryOrganizationReference property.
     * 
     * @return
     *     possible object is
     *     {@link SupervisoryOrganizationObjectType }
     *     
     */
    public SupervisoryOrganizationObjectType getSupervisoryOrganizationReference() {
        return supervisoryOrganizationReference;
    }

    /**
     * Sets the value of the supervisoryOrganizationReference property.
     * 
     * @param value
     *     allowed object is
     *     {@link SupervisoryOrganizationObjectType }
     *     
     */
    public void setSupervisoryOrganizationReference(SupervisoryOrganizationObjectType value) {
        this.supervisoryOrganizationReference = value;
    }

    /**
     * Gets the value of the positionRequestReasonReference property.
     * 
     * @return
     *     possible object is
     *     {@link EventClassificationSubcategoryObjectType }
     *     
     */
    public EventClassificationSubcategoryObjectType getPositionRequestReasonReference() {
        return positionRequestReasonReference;
    }

    /**
     * Sets the value of the positionRequestReasonReference property.
     * 
     * @param value
     *     allowed object is
     *     {@link EventClassificationSubcategoryObjectType }
     *     
     */
    public void setPositionRequestReasonReference(EventClassificationSubcategoryObjectType value) {
        this.positionRequestReasonReference = value;
    }

    /**
     * Gets the value of the positionData property.
     * 
     * @return
     *     possible object is
     *     {@link PositionDefinitionDataType }
     *     
     */
    public PositionDefinitionDataType getPositionData() {
        return positionData;
    }

    /**
     * Sets the value of the positionData property.
     * 
     * @param value
     *     allowed object is
     *     {@link PositionDefinitionDataType }
     *     
     */
    public void setPositionData(PositionDefinitionDataType value) {
        this.positionData = value;
    }

    /**
     * Gets the value of the qualificationReplacementData property.
     * 
     * @return
     *     possible object is
     *     {@link QualificationDataForPositionRestrictionOrJobProfileType }
     *     
     */
    public QualificationDataForPositionRestrictionOrJobProfileType getQualificationReplacementData() {
        return qualificationReplacementData;
    }

    /**
     * Sets the value of the qualificationReplacementData property.
     * 
     * @param value
     *     allowed object is
     *     {@link QualificationDataForPositionRestrictionOrJobProfileType }
     *     
     */
    public void setQualificationReplacementData(QualificationDataForPositionRestrictionOrJobProfileType value) {
        this.qualificationReplacementData = value;
    }

    /**
     * Gets the value of the positionGroupRestrictionsData property.
     * 
     * @return
     *     possible object is
     *     {@link PositionGroupRestrictionDataType }
     *     
     */
    public PositionGroupRestrictionDataType getPositionGroupRestrictionsData() {
        return positionGroupRestrictionsData;
    }

    /**
     * Sets the value of the positionGroupRestrictionsData property.
     * 
     * @param value
     *     allowed object is
     *     {@link PositionGroupRestrictionDataType }
     *     
     */
    public void setPositionGroupRestrictionsData(PositionGroupRestrictionDataType value) {
        this.positionGroupRestrictionsData = value;
    }

    /**
     * Gets the value of the editAssignOrganizationSubProcess property.
     * 
     * @return
     *     possible object is
     *     {@link EditAssignPositionOrganizationSubBusinessProcessType }
     *     
     */
    public EditAssignPositionOrganizationSubBusinessProcessType getEditAssignOrganizationSubProcess() {
        return editAssignOrganizationSubProcess;
    }

    /**
     * Sets the value of the editAssignOrganizationSubProcess property.
     * 
     * @param value
     *     allowed object is
     *     {@link EditAssignPositionOrganizationSubBusinessProcessType }
     *     
     */
    public void setEditAssignOrganizationSubProcess(EditAssignPositionOrganizationSubBusinessProcessType value) {
        this.editAssignOrganizationSubProcess = value;
    }

    /**
     * Gets the value of the requestDefaultCompensationSubProcess property.
     * 
     * @return
     *     possible object is
     *     {@link RequestCompensationDefaultSubBusinessProcessType }
     *     
     */
    public RequestCompensationDefaultSubBusinessProcessType getRequestDefaultCompensationSubProcess() {
        return requestDefaultCompensationSubProcess;
    }

    /**
     * Sets the value of the requestDefaultCompensationSubProcess property.
     * 
     * @param value
     *     allowed object is
     *     {@link RequestCompensationDefaultSubBusinessProcessType }
     *     
     */
    public void setRequestDefaultCompensationSubProcess(RequestCompensationDefaultSubBusinessProcessType value) {
        this.requestDefaultCompensationSubProcess = value;
    }

    /**
     * Gets the value of the assignPayGroupSubProcess property.
     * 
     * @return
     *     possible object is
     *     {@link AssignPayGroupForPositionRestrictionsSubBusinessProcessType }
     *     
     */
    public AssignPayGroupForPositionRestrictionsSubBusinessProcessType getAssignPayGroupSubProcess() {
        return assignPayGroupSubProcess;
    }

    /**
     * Sets the value of the assignPayGroupSubProcess property.
     * 
     * @param value
     *     allowed object is
     *     {@link AssignPayGroupForPositionRestrictionsSubBusinessProcessType }
     *     
     */
    public void setAssignPayGroupSubProcess(AssignPayGroupForPositionRestrictionsSubBusinessProcessType value) {
        this.assignPayGroupSubProcess = value;
    }

    /**
     * Gets the value of the assignCostingAllocationSubProcess property.
     * 
     * @return
     *     possible object is
     *     {@link AssignCostingAllocationSubBusinessProcessType }
     *     
     */
    public AssignCostingAllocationSubBusinessProcessType getAssignCostingAllocationSubProcess() {
        return assignCostingAllocationSubProcess;
    }

    /**
     * Sets the value of the assignCostingAllocationSubProcess property.
     * 
     * @param value
     *     allowed object is
     *     {@link AssignCostingAllocationSubBusinessProcessType }
     *     
     */
    public void setAssignCostingAllocationSubProcess(AssignCostingAllocationSubBusinessProcessType value) {
        this.assignCostingAllocationSubProcess = value;
    }

}
