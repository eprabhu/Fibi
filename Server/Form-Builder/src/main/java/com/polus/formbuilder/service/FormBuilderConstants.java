package com.polus.formbuilder.service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.stereotype.Component;

@Component
public final class FormBuilderConstants {

	static final String OPA_MODULE = "23";

	static final String CONSULTING_DISCLOSURE_MODULE = "27";

	static final String QUESTIONNAIR_COMPONENT = "QN";

	static final String CUSTOM_ELEMENT_COMPONENT = "CE";

	static final String PROGRAMMED_ELEMENT_COMPONENT = "PE";

	public static final String INSERT = "I";

	public static final String UPDATE = "U";

	public static final String DELETE = "D";

	public static final String CHECK_BOX = "CB";

	public static final String RADIO_BUTTON = "RB";

	public static final String STRING_ELEMENT = "SE";

	public static final String NUMBER_ELEMENT = "NE";

	public static final String USER_DROPDOWN = "UD";

	public static final String SYSTEM_DROPDOWN = "SD";

	public static final String AUTO_SUGGEST = "AS";

	public static final String DATE_ELEMENT = "DE";

	public static final String ELASTIC_SEARCH = "ES";

	public static final String TEXT_ELEMENT = "TE";

    public static final List<String> CUSTOM_ELEMENT_COMPONENT_LIST;

    static {
        List<String> componentList = new ArrayList<>();
        componentList.add(NUMBER_ELEMENT);
        componentList.add(DATE_ELEMENT);
        componentList.add(AUTO_SUGGEST);
        componentList.add(CHECK_BOX);
        componentList.add(ELASTIC_SEARCH);
        componentList.add(RADIO_BUTTON);
        componentList.add(STRING_ELEMENT);
        componentList.add(SYSTEM_DROPDOWN);
        componentList.add(TEXT_ELEMENT);
        componentList.add(USER_DROPDOWN);
        componentList.add(CUSTOM_ELEMENT_COMPONENT);

        CUSTOM_ELEMENT_COMPONENT_LIST = Collections.unmodifiableList(componentList);
    }

    public static final Map<String, String> CUSTOM_DATA_TYPE_CODES;

    static {
        Map<String, String> customDataTypeCodes = new HashMap<>();
        customDataTypeCodes.put(STRING_ELEMENT, "1");
        customDataTypeCodes.put(NUMBER_ELEMENT, "2");
        customDataTypeCodes.put(DATE_ELEMENT, "3");
        customDataTypeCodes.put(CHECK_BOX, "4");
        customDataTypeCodes.put(RADIO_BUTTON, "5");
        customDataTypeCodes.put(ELASTIC_SEARCH, "6");
        customDataTypeCodes.put(AUTO_SUGGEST, "7");
        customDataTypeCodes.put(SYSTEM_DROPDOWN, "8");
        customDataTypeCodes.put(USER_DROPDOWN, "9");
        customDataTypeCodes.put(TEXT_ELEMENT, "10");

        CUSTOM_DATA_TYPE_CODES = Collections.unmodifiableMap(customDataTypeCodes);
    }

}
