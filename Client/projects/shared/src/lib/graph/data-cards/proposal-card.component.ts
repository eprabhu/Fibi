import { Component, Input } from '@angular/core';
import { DataService } from '../data.service';

@Component({
  standalone: true,
  selector: 'app-proposal-card',
  template: `
       <div class="data-grid">
        <div class="mr-15">
            <img class="profile_img" [src]="imagePath" alt="an icon for representing a proposal">
        </div>
        <div class="ml-15 mr-8">
            <h2 class="heading">{{data.title}}({{data.proposal_id}})</h2>
            <p class="sub-heading">{{data.pi_name}} (PI)</p>
        </div>
        <div class="align-items-end d-flex ms-auto">
          <button (click)="openProposalDetails(data.proposal_id)"
          class="align-items-center btn btn-primary d-flex fs-14"
          title="Click to view Proposal Details"
          area-describedby="Click to view Proposal Details">
          <i class="fa fa-eye mr-2" aria-hidden="true"></i>
          View
          </button>
        </div>
      </div>
      <div>
          <p class="sub-heading">
            <span> <i aria-hidden="true" class="fa fa-calendar fa-large"></i></span>
            {{data.start_date}} - {{data.end_date}}
          </p>
          <p class="sub-heading">
            <span > <i aria-hidden="true" class="fa fa-home fa-large"></i></span>
            {{data.lead_unit_name}}({{data.unit_number}})</p>
          <p class="sub-heading">
            <span>
            <img class="" src="/assets/images/sponsor.png" alt="an icon for represrnting a person">
            </span> {{data.sponsor_name}}({{data.sponsor_code}})</p>
      </div>
  `,
  styles: [
    `.profile_img {
      width: 50px;
      height: 50px;
      object-fit: cover;
      object-position: 50% 50%;
    }
    .data-grid {
      display: flex;
      flex-direction: row;
    }
    .heading {
      font-family: "Poppins";
      font-weight: 500;
      font-size: 18px;
      margin: 3px 0;
    }
    .sub-heading {
      font-family: "Avenir";
      font-size: 14px;
      color: rgba(34, 34, 34, 0.5);
      margin: 5px 0;
    }
    .sub-heading span img {
      height:18px;
      width:18px;
    }
    .sub-heading span {
      margin-right: 2rem;
    }
    .sub-heading span i {
      font-size: 2rem;
    }
  `],
})
export class ProposalCardComponent {

  @Input() data: any = {};
  @Input() imagePath: any = {};

  constructor(public graphDataService: DataService) {}

  openProposalDetails(id) {
    this.graphDataService.openDetailsEvent.next({'node': 'Proposal', 'id': id});
  }

}
