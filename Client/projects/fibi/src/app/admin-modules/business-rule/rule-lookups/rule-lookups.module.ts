import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RuleLookupsComponent } from './rule-lookups.component';
import { RouterModule } from '@angular/router';
import { SharedModule } from '../../../shared/shared.module';
import { RuleViewModule } from '../rule-view/rule-view.module';

@NgModule({
  imports: [
    CommonModule,
    RouterModule.forChild([{path: '', component: RuleLookupsComponent}]),
    SharedModule,
    RuleViewModule
  ],
  declarations: [RuleLookupsComponent]
})
export class RuleLookupsModule { }
