import {Component, OnInit} from '@angular/core';
import {environment} from "../../../../admin-dashboard/src/environments/environment";
import {CommonService} from "../common/services/common.service";
import { fadeInOutHeight } from 'projects/fibi/src/app/common/utilities/animations';

@Component({
  selector: 'app-configuration',
  templateUrl: './configuration.component.html',
  styleUrls: ['./configuration.component.scss'],
  animations: [fadeInOutHeight]
})
export class ConfigurationComponent implements OnInit{

  deployMap = environment.deployUrl;
  isMaintainQuestionnaire = false;
  isApplicationAdministrator = false;
  isMaintainUserRoles = false;
  isMaintainRole = false;
  isMaintainPerson = false;
  isMaintainOrcidWorks = false;
  isMaintainDelegation = false;
  isMaintainTimeSheet = false;
  isViewTimeSheet = false;
  isMaintainTraining = false;

  constructor(private _commonService: CommonService) {
  }

  ngOnInit() {
    this.setAdminRights();
  }

  openInNewTab(path: string) {
    const url = this._commonService.fibiApplicationUrl + '#' + path;
    window.open(url);
  }

  setAdminRights() {
    this.isMaintainQuestionnaire = this._commonService.getAvailableRight(['MAINTAIN_QUESTIONNAIRE']);
    this.isApplicationAdministrator = this._commonService.getAvailableRight(['APPLICATION_ADMINISTRATOR']);
    this.isMaintainUserRoles = this._commonService.getAvailableRight(['MAINTAIN_USER_ROLES']);
    this.isMaintainRole = this._commonService.getAvailableRight(['MAINTAIN_ROLE']);
    this.isMaintainPerson = this._commonService.getAvailableRight(['MAINTAIN_PERSON']);
    this.isMaintainOrcidWorks = this._commonService.getAvailableRight(['MAINTAIN_ORCID_WORKS']);
    this.isMaintainDelegation = this._commonService.getAvailableRight(['MAINTAIN_DELEGATION']);
    this.isMaintainTimeSheet = this._commonService.getAvailableRight(['MAINTAIN_KEY_PERSON_TIMESHEET']);
    this.isViewTimeSheet = this._commonService.getAvailableRight(['VIEW_KEY_PERSON_TIMESHEET']);
    this.isMaintainTraining = this._commonService.getAvailableRight(['MAINTAIN_TRAINING']);
  }

}
