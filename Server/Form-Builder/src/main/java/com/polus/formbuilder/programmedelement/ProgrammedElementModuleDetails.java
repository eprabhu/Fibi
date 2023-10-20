package com.polus.formbuilder.programmedelement;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ProgrammedElementModuleDetails{

	 private  String moduleItemCode;
	 private  String moduleSubItemCode;
	 private  String moduleItemKey;
	 private  String moduleSubItemKey;
	 private  String loggedInUser;
}
