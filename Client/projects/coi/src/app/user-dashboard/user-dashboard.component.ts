import {Component, OnDestroy, OnInit} from '@angular/core';
import {UserDashboardService} from "./user-dashboard.service";
import {subscriptionHandler} from "../../../../fibi/src/app/common/utilities/subscription-handler";

@Component({
  selector: 'app-user-dashboard',
  templateUrl: './user-dashboard.component.html',
  styleUrls: ['./user-dashboard.component.scss'],
  providers: [UserDashboardService]
})
export class UserDashboardComponent implements OnInit, OnDestroy{
  $subscriptions = [];
  constructor(public service: UserDashboardService) {
  }

  ngOnInit(): void {
    this.getActiveDisclosure();
  }

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }

  getActiveDisclosure() {
    this.$subscriptions.push(this.service.getActiveDisclosure().subscribe((res: any) => {
      this.service.activeDisclosures = res.coiDisclosures || [];
    }))
  }
}
