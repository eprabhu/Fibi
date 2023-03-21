//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.11 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2020.11.25 at 01:02:19 PM IST 
//


package com.polus.fibicomp.manpowerintegration.dto.recruiting;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * Encapsulating element containing all Calculated Plan Compensation data.
 * 
 * <p>Java class for Proposed_Calculated_Plan_Assignment_Container_DataType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Proposed_Calculated_Plan_Assignment_Container_DataType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="Calculated_Plan_Sub_Data" type="{urn:com.workday/bsvc}Proposed_Calculated_Plan_Assignment_DataType" maxOccurs="unbounded" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute name="Replace" type="{http://www.w3.org/2001/XMLSchema}boolean" /&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Proposed_Calculated_Plan_Assignment_Container_DataType", propOrder = {
    "calculatedPlanSubData"
})
public class ProposedCalculatedPlanAssignmentContainerDataType {

    @XmlElement(name = "Calculated_Plan_Sub_Data")
    protected List<ProposedCalculatedPlanAssignmentDataType> calculatedPlanSubData;
    @XmlAttribute(name = "Replace", namespace = "urn:com.workday/bsvc")
    protected Boolean replace;

    /**
     * Gets the value of the calculatedPlanSubData property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the calculatedPlanSubData property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getCalculatedPlanSubData().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ProposedCalculatedPlanAssignmentDataType }
     * 
     * 
     */
    public List<ProposedCalculatedPlanAssignmentDataType> getCalculatedPlanSubData() {
        if (calculatedPlanSubData == null) {
            calculatedPlanSubData = new ArrayList<ProposedCalculatedPlanAssignmentDataType>();
        }
        return this.calculatedPlanSubData;
    }

    /**
     * Gets the value of the replace property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isReplace() {
        return replace;
    }

    /**
     * Sets the value of the replace property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setReplace(Boolean value) {
        this.replace = value;
    }

}
