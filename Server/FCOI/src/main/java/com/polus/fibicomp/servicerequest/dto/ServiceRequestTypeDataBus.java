package com.polus.fibicomp.servicerequest.dto;

import java.util.ArrayList;
import java.util.List;

public class ServiceRequestTypeDataBus {

	public ServiceRequestTypeDataBus() {
		serviceRequestTypes = new ArrayList<ServiceRequestType>();
	}

	private List<ServiceRequestType> serviceRequestTypes;

	public List<ServiceRequestType> getServiceRequestTypes() {
		return serviceRequestTypes;
	}

	public void setServiceRequestTypes(List<ServiceRequestType> serviceRequestTypes) {
		this.serviceRequestTypes = serviceRequestTypes;
	}

	
}
