/**
 * created by Archana R on 20/11/2019
 * last updated by Archana R  on 21/11/2019
 */

import { Component, OnInit, OnDestroy } from '@angular/core';
import { CommonDataService } from '../../services/common-data.service';
import { RoleService } from '../role.service';
import { Subscription } from 'rxjs';
import { subscriptionHandler } from '../../../common/utilities/subscription-handler';

@Component({
  selector: 'app-role-view',
  templateUrl: './role-view.component.html',
  styleUrls: ['./role-view.component.css']
})
export class RoleViewComponent implements OnInit, OnDestroy {

  isCollapseList = [];
  roleList: any = [];
  person: any = {};
  personRolesList = [];
  awardData: any;
  $subscriptions: Subscription[] = [];

  constructor(private _commonData: CommonDataService, public _roleService: RoleService) { }

  ngOnInit() {
    this.$subscriptions.push(this._commonData.awardData.subscribe((data: any) => {
      if (data) {
        this.awardData = data.award;
      }
    }));
    this.fetchPersonRoles();
  }

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }
  /**
 * Loads the role list and person role list
 */
  fetchPersonRoles() {
    this.$subscriptions.push(this._roleService.fetchAwardPersonRoles
      ({ 'awardId': this.awardData.awardId })
      .subscribe((data: any) => {
        this.roleList = data.moduleDerivedRoles;
        this.personRolesList = data.awardPersonRoles ? data.awardPersonRoles : [];
        this.roleList.forEach(element => {
          this.isCollapseList[element.roleName] = true;
        });
      }));
    this.$subscriptions.push(this._commonData.awardData.subscribe((data: any) => {
      if (data) {
        this.awardData = data.award;
      }
    }));
  }
  /**
   * @param  {} roleName
   * to set collapsable tables
   */
  collapseRoleTables(roleName) {
    this.isCollapseList[roleName] === false ? this.isCollapseList[roleName] = true : this.isCollapseList[roleName] = false;
  }
  /**
* @param  {} roleId
* returns the list persons which comes under the role id
*/
  filterPersonPerRole(roleId) {
    if (this.personRolesList.length > 0) {
      return this.personRolesList.filter(person => (person.roleId === roleId));
    }
  }
}
