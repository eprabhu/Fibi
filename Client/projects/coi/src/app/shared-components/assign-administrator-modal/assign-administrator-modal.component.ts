import { Component, EventEmitter, Input, OnChanges, OnInit, Output } from '@angular/core';
import { CompleterOptions } from '../../../../../fibi/src/app/service-request/service-request.interface';
import { CommonService } from '../../common/services/common.service';
import { Subscription } from 'rxjs';
import { AssignAdministratorModalService } from './assign-administrator-modal.service';

declare var $: any;
@Component({
  selector: 'app-assign-administrator-modal',
  templateUrl: './assign-administrator-modal.component.html',
  styleUrls: ['./assign-administrator-modal.component.scss'],
  providers: [AssignAdministratorModalService]
})
export class AssignAdministratorModalComponent implements OnChanges {

  isAssignToMe = false;
  adminSearchOptions: any = {};
  clearAdministratorField: String;
  assignAdminMap = new Map();
  addAdmin: any = {};
  adminGroupsCompleterOptions: CompleterOptions = new CompleterOptions();
  clearAdminGroupField: any;
  isShowWarningMessage = false;
  warningMessage: any;
  $subscriptions: Subscription[] = [];
  isSaving = false;
  @Input() disclosureId = null;
  @Output() closeModal: EventEmitter<any> = new EventEmitter<any>();


  constructor( private _commonService: CommonService, 
               private _assignAdminService: AssignAdministratorModalService ) { }

  ngOnChanges() {
    this.getAdminDetails();
    this.addAdmin.disclosureId = this.disclosureId ;
  }

  getAdminDetails() {
    this.$subscriptions.push(this._assignAdminService.getAdminDetails().subscribe((data: any) => {
        this.setAdminGroupOptions(data);
        this.setCompleterOptions(this.adminSearchOptions, data.persons, 'fullName');
    }));
}

private setAdminGroupOptions(data): void {
  this.adminGroupsCompleterOptions = {
      arrayList: this.getActiveAdminGroups(data),
      contextField: 'adminGroupName',
      filterFields: 'adminGroupName',
      formatString: 'adminGroupName',
      defaultValue: ''
  };
}

private getActiveAdminGroups(data) {
  return data.adminGroups.filter(element => element.isActive === 'Y');
}

setCompleterOptions(searchOption: any = null, arrayList: any, searchShowField: string) {
  searchOption.defaultValue = '';
  searchOption.arrayList = arrayList || [];
  searchOption.contextField = searchShowField;
  searchOption.filterFields = searchShowField;
  searchOption.formatString = searchShowField;
}
  public assignToMe(checkBoxEvent: any) {
    if (checkBoxEvent.target.checked) {
      this.adminSearchOptions.defaultValue = this._commonService.getCurrentUserDetail('fullName');
      this.clearAdministratorField = new String('false');
      this.addAdmin.adminPersonId = this._commonService.getCurrentUserDetail('personId');
      this.getAdminGroupDetails(this._commonService.getCurrentUserDetail('personId'));
      this.isAssignToMe = true;
      this.assignAdminMap.clear();
    } else {
      this.clearAdministratorField = new String('true');
      this.clearAdminGroupField = new String('true');
      this.addAdmin.adminPersonId = null;
      this.isAssignToMe = false;
    }
  }

  public adminSelect(event: any) {
    if (event) {
      this.getAdminGroupDetails(event.personId);
      this.addAdmin.adminPersonId = event.personId;
      this.isAssignToMe = this.setAssignToMe();
      this.assignAdminMap.clear();
    } else {
      this.addAdmin.adminGroupId = null;
      this.addAdmin.adminPersonId = null;
      this.clearAdminGroupField = new String('true');
      this.isAssignToMe = false;
      this.isShowWarningMessage = false;
    }
  }

  public adminGroupSelect(event) {
    if (event) {
      this.isShowWarningMessage = false;
      this.addAdmin.adminGroupId = event.adminGroupId;
    } else {
      this.addAdmin.adminGroupId = null;
    }
  }

  public assignAdministrator() {
    if (!this.isSaving && this.validateAdmin()) {
      this.isSaving = true;
      this.addAdmin.disclosureId = this.disclosureId;
      this.$subscriptions.push(this._assignAdminService.assignAdmin(
        this.addAdmin
      ).subscribe((data: any) => {
        this.isAssignToMe = false;
        this.isShowWarningMessage = false;
        this.addAdmin = {};
        this.isSaving = false;
        this.clearAdministratorField = new String('true');
        this.closeModal.emit(data);
        document.getElementById('hide-assign-admin').click();
      }, err => {
        this.isSaving = false;
      }));
    }
  }

  private validateAdmin(): boolean {
    this.assignAdminMap.clear();
    if (!this.addAdmin.adminPersonId) {
      this.assignAdminMap.set('adminName', 'adminName');
    }
    return this.assignAdminMap.size > 0 ? false : true;
  }

  private getAdminGroupDetails(personId) {
    this.$subscriptions.push(this._assignAdminService.getPersonGroup(personId).subscribe((data: any) => {
      if (data.adminGroupId) {
        this.clearAdminGroupField = new String('false');
        this.addAdmin.adminGroupId = data.adminGroupId;
        this.isShowWarningMessage = false;
      } else {
        this.isShowWarningMessage = true;
        this.warningMessage = data;
      }
    }));
  }

    private setAssignToMe(): boolean {
        return this.addAdmin.adminPersonId === this._commonService.getCurrentUserDetail('personId') ? true : false;
    }

    public clearData() {
        this.isAssignToMe = false;
        this.addAdmin = {};
        this.isShowWarningMessage = false;
        this.clearAdminGroupField = new String('true');
        this.clearAdministratorField = new String('true');
    }
}
