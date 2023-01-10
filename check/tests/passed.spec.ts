import { get_account, set_mockup, set_quiet } from '@completium/experiment-ts';
import assert from 'assert'
import { Address, Nat } from '@completium/archetype-ts-types';

import * as add_update_record from '../bindings/passed/add_update_record'
import * as addupdate_partition from '../bindings/passed/addupdate_partition'
import * as addupdate_partition2 from '../bindings/passed/addupdate_partition2'
import * as addupdate_partition_with_no_effect_on_default_value from '../bindings/passed/addupdate_partition_with_no_effect_on_default_value'
import * as addupdate_with_no_effect_on_default_value from '../bindings/passed/addupdate_with_no_effect_on_default_value'
import * as annot_enum from '../bindings/passed/annot_enum'
import * as apply_lambda from '../bindings/passed/apply_lambda'
import * as arith_bls from '../bindings/passed/arith_bls'
import * as arith_tez from '../bindings/passed/arith_tez'
import * as ascii_string from '../bindings/passed/ascii_string'
import * as asset_access from '../bindings/passed/asset_access'
import * as asset_access_basic from '../bindings/passed/asset_access_basic'
import * as asset_access_option_found from '../bindings/passed/asset_access_option_found'
import * as asset_access_option_not_found from '../bindings/passed/asset_access_option_not_found'
import * as asset_access_value from '../bindings/passed/asset_access_value'
import * as asset_addupdate from '../bindings/passed/asset_addupdate'
import * as asset_big_map from '../bindings/passed/asset_big_map'
import * as asset_big_map_unit_effect_add from '../bindings/passed/asset_big_map_unit_effect_add'
import * as asset_big_map_unit_effect_addupdate from '../bindings/passed/asset_big_map_unit_effect_addupdate'
import * as asset_big_map_unit_effect_remove from '../bindings/passed/asset_big_map_unit_effect_remove'
import * as asset_big_map_unit_effect_removeall from '../bindings/passed/asset_big_map_unit_effect_removeall'
import * as asset_big_map_unit_effect_update from '../bindings/passed/asset_big_map_unit_effect_update'
import * as asset_big_map_unit_expression_contains from '../bindings/passed/asset_big_map_unit_expression_contains'
import * as asset_big_map_unit_storage from '../bindings/passed/asset_big_map_unit_storage'
import * as asset_for from '../bindings/passed/asset_for'
import * as asset_init_by_const_key from '../bindings/passed/asset_init_by_const_key'
import * as asset_init_by_const_key_parameter from '../bindings/passed/asset_init_by_const_key_parameter'
import * as asset_initializedby_aggregate_empty from '../bindings/passed/asset_initializedby_aggregate_empty'
import * as asset_initializedby_aggregate_filled from '../bindings/passed/asset_initializedby_aggregate_filled'
import * as asset_key_in_record from '../bindings/passed/asset_key_in_record'
import * as asset_key_tuple from '../bindings/passed/asset_key_tuple'
import * as asset_not_found from '../bindings/passed/asset_not_found'
import * as asset_nth from '../bindings/passed/asset_nth'
import * as asset_put_single from '../bindings/passed/asset_put_single'
import * as asset_simple from '../bindings/passed/asset_simple'
import * as asset_simple_to_big_map from '../bindings/passed/asset_simple_to_big_map'
import * as asset_tern_opt from '../bindings/passed/asset_tern_opt'
import * as asset_ternary_expr_found from '../bindings/passed/asset_ternary_expr_found'
import * as asset_ternary_expr_notfound from '../bindings/passed/asset_ternary_expr_notfound'
import * as asset_types_get from '../bindings/passed/asset_types_get'
import * as asset_update_with_basic_container_map_lit_add from '../bindings/passed/asset_update_with_basic_container_map_lit_add'
import * as asset_update_with_basic_container_map_lit_remove from '../bindings/passed/asset_update_with_basic_container_map_lit_remove'
import * as asset_update_with_basic_container_map_var_list_add from '../bindings/passed/asset_update_with_basic_container_map_var_list_add'
import * as asset_update_with_basic_container_map_var_list_remove from '../bindings/passed/asset_update_with_basic_container_map_var_list_remove'
import * as asset_update_with_basic_container_map_var_set_add from '../bindings/passed/asset_update_with_basic_container_map_var_set_add'
import * as asset_update_with_basic_container_map_var_set_remove from '../bindings/passed/asset_update_with_basic_container_map_var_set_remove'
import * as asset_update_with_basic_container_set_lit_add from '../bindings/passed/asset_update_with_basic_container_set_lit_add'
import * as asset_update_with_basic_container_set_lit_remove from '../bindings/passed/asset_update_with_basic_container_set_lit_remove'
import * as asset_update_with_basic_container_set_var_list_add from '../bindings/passed/asset_update_with_basic_container_set_var_list_add'
import * as asset_update_with_basic_container_set_var_list_remove from '../bindings/passed/asset_update_with_basic_container_set_var_list_remove'
import * as asset_update_with_basic_container_set_var_set_add from '../bindings/passed/asset_update_with_basic_container_set_var_set_add'
import * as asset_update_with_basic_container_set_var_set_remove from '../bindings/passed/asset_update_with_basic_container_set_var_set_remove'
import * as assign_add_record from '../bindings/passed/assign_add_record'
import * as assign_add_tuple from '../bindings/passed/assign_add_tuple'
import * as assign_field from '../bindings/passed/assign_field'
import * as assign_minus_nat from '../bindings/passed/assign_minus_nat'
import * as assign_opt from '../bindings/passed/assign_opt'
import * as assign_var_rat_int from '../bindings/passed/assign_var_rat_int'
import * as assign_vardecl_rat_int from '../bindings/passed/assign_vardecl_rat_int'
import * as assign_vardecl_rat_nat from '../bindings/passed/assign_vardecl_rat_nat'
import * as before_asset_api from '../bindings/passed/before_asset_api'
import * as before_var from '../bindings/passed/before_var'
import * as builtin_in_function from '../bindings/passed/builtin_in_function'
import * as called_by_an_asset from '../bindings/passed/called_by_an_asset'
import * as cast from '../bindings/passed/cast'
import * as cast_dur_int from '../bindings/passed/cast_dur_int'
import * as cast_nat_int from '../bindings/passed/cast_nat_int'
import * as cast_nat_int_lit from '../bindings/passed/cast_nat_int_lit'
import * as cast_return from '../bindings/passed/cast_return'
import * as cast_view_pklist from '../bindings/passed/cast_view_pklist'
import * as col_iter_direct_storage from '../bindings/passed/col_iter_direct_storage'
import * as col_iter_filter_storage from '../bindings/passed/col_iter_filter_storage'
import * as compare_enum from '../bindings/passed/compare_enum'
import * as const_decl from '../bindings/passed/const_decl'
import * as contract_called from '../bindings/passed/contract_called'
import * as contract_caller from '../bindings/passed/contract_caller'
import * as contract_empty from '../bindings/passed/contract_empty'
import * as contract_to_address from '../bindings/passed/contract_to_address'
import * as contract_transition from '../bindings/passed/contract_transition'
import * as contract_transition_on_asset from '../bindings/passed/contract_transition_on_asset'
import * as counter from '../bindings/passed/counter'
import * as counter_proxy from '../bindings/passed/counter_proxy'
import * as custom_args_with_record from '../bindings/passed/custom_args_with_record'
import * as custom_storage from '../bindings/passed/custom_storage'
import * as custom_storage10 from '../bindings/passed/custom_storage10'
import * as custom_storage2 from '../bindings/passed/custom_storage2'
import * as custom_storage3 from '../bindings/passed/custom_storage3'
import * as custom_storage4 from '../bindings/passed/custom_storage4'
import * as custom_storage5 from '../bindings/passed/custom_storage5'
import * as custom_storage6 from '../bindings/passed/custom_storage6'
import * as custom_storage7 from '../bindings/passed/custom_storage7'
import * as custom_storage8 from '../bindings/passed/custom_storage8'
import * as custom_storage9 from '../bindings/passed/custom_storage9'
import * as dangling_else from '../bindings/passed/dangling_else'
import * as dec_lit from '../bindings/passed/dec_lit'
import * as decl_var_opt from '../bindings/passed/decl_var_opt'
import * as decl_var_opt_default from '../bindings/passed/decl_var_opt_default'
import * as decomp_if from '../bindings/passed/decomp_if'
import * as decomp_if2 from '../bindings/passed/decomp_if2'
import * as decomp_if3 from '../bindings/passed/decomp_if3'
import * as decomp_if4 from '../bindings/passed/decomp_if4'
import * as decomp_ifexpr from '../bindings/passed/decomp_ifexpr'
import * as decomp_map from '../bindings/passed/decomp_map'
import * as decomp_test from '../bindings/passed/decomp_test'
import * as decomp_test2 from '../bindings/passed/decomp_test2'
import * as decomp_while from '../bindings/passed/decomp_while'
import * as decomp_while1 from '../bindings/passed/decomp_while1'
import * as decomp_while2 from '../bindings/passed/decomp_while2'
import * as duration_to_int from '../bindings/passed/duration_to_int'
import * as effect_add_asset_with_complex_partition from '../bindings/passed/effect_add_asset_with_complex_partition'
import * as effect_control_for_aggregate from '../bindings/passed/effect_control_for_aggregate'
import * as effect_control_for_collection from '../bindings/passed/effect_control_for_collection'
import * as effect_control_for_collection_one_field from '../bindings/passed/effect_control_for_collection_one_field'
import * as effect_control_for_list from '../bindings/passed/effect_control_for_list'
import * as effect_control_for_map from '../bindings/passed/effect_control_for_map'
import * as effect_control_for_partition from '../bindings/passed/effect_control_for_partition'
import * as effect_control_for_set from '../bindings/passed/effect_control_for_set'
import * as effect_control_for_view from '../bindings/passed/effect_control_for_view'
import * as effect_control_if from '../bindings/passed/effect_control_if'
import * as effect_control_if_else from '../bindings/passed/effect_control_if_else'
import * as effect_control_iter from '../bindings/passed/effect_control_iter'
import * as effect_control_iter_init from '../bindings/passed/effect_control_iter_init'
import * as effect_control_match_enum from '../bindings/passed/effect_control_match_enum'
import * as effect_control_match_list from '../bindings/passed/effect_control_match_list'
import * as effect_control_match_option from '../bindings/passed/effect_control_match_option'
import * as effect_control_match_or from '../bindings/passed/effect_control_match_or'
import * as effect_control_matchwith from '../bindings/passed/effect_control_matchwith'
import * as effect_control_sequence from '../bindings/passed/effect_control_sequence'
import * as effect_control_while from '../bindings/passed/effect_control_while'
import * as effect_dofailif from '../bindings/passed/effect_dofailif'
import * as effect_dorequire from '../bindings/passed/effect_dorequire'
import * as effect_dorequire_not from '../bindings/passed/effect_dorequire_not'
import * as effect_fail from '../bindings/passed/effect_fail'
import * as effect_instruction_put_in_asset from '../bindings/passed/effect_instruction_put_in_asset'
import * as effect_method_asset_add_aggregate from '../bindings/passed/effect_method_asset_add_aggregate'
import * as effect_method_asset_add_asset from '../bindings/passed/effect_method_asset_add_asset'
import * as effect_method_asset_add_asset2 from '../bindings/passed/effect_method_asset_add_asset2'
import * as effect_method_asset_add_asset_one_field from '../bindings/passed/effect_method_asset_add_asset_one_field'
import * as effect_method_asset_add_asset_with_aggregate from '../bindings/passed/effect_method_asset_add_asset_with_aggregate'
import * as effect_method_asset_add_asset_with_partition from '../bindings/passed/effect_method_asset_add_asset_with_partition'
import * as effect_method_asset_add_asset_with_partition_2 from '../bindings/passed/effect_method_asset_add_asset_with_partition_2'
import * as effect_method_asset_add_partition from '../bindings/passed/effect_method_asset_add_partition'
import * as effect_method_asset_add_partition_one_field from '../bindings/passed/effect_method_asset_add_partition_one_field'
import * as effect_method_asset_addupdate from '../bindings/passed/effect_method_asset_addupdate'
import * as effect_method_asset_addupdate_partition from '../bindings/passed/effect_method_asset_addupdate_partition'
import * as effect_method_asset_addupdate_with_add_aggregate from '../bindings/passed/effect_method_asset_addupdate_with_add_aggregate'
import * as effect_method_asset_addupdate_with_add_map from '../bindings/passed/effect_method_asset_addupdate_with_add_map'
import * as effect_method_asset_addupdate_with_add_map_var from '../bindings/passed/effect_method_asset_addupdate_with_add_map_var'
import * as effect_method_asset_addupdate_with_add_partition from '../bindings/passed/effect_method_asset_addupdate_with_add_partition'
import * as effect_method_asset_addupdate_with_add_set from '../bindings/passed/effect_method_asset_addupdate_with_add_set'
import * as effect_method_asset_addupdate_with_remove_map from '../bindings/passed/effect_method_asset_addupdate_with_remove_map'
import * as effect_method_asset_addupdate_with_remove_set from '../bindings/passed/effect_method_asset_addupdate_with_remove_set'
import * as effect_method_asset_addupdate_with_replace_aggregate from '../bindings/passed/effect_method_asset_addupdate_with_replace_aggregate'
import * as effect_method_asset_addupdate_with_replace_partition from '../bindings/passed/effect_method_asset_addupdate_with_replace_partition'
import * as effect_method_asset_big_map_0_put_remove_put from '../bindings/passed/effect_method_asset_big_map_0_put_remove_put'
import * as effect_method_asset_big_map_0_put_remove_remove from '../bindings/passed/effect_method_asset_big_map_0_put_remove_remove'
import * as effect_method_asset_big_map_1_put_remove_put from '../bindings/passed/effect_method_asset_big_map_1_put_remove_put'
import * as effect_method_asset_big_map_1_put_remove_remove from '../bindings/passed/effect_method_asset_big_map_1_put_remove_remove'
import * as effect_method_asset_big_map_2_put_remove_put from '../bindings/passed/effect_method_asset_big_map_2_put_remove_put'
import * as effect_method_asset_big_map_2_put_remove_remove from '../bindings/passed/effect_method_asset_big_map_2_put_remove_remove'
import * as effect_method_asset_clear_view from '../bindings/passed/effect_method_asset_clear_view'
import * as effect_method_asset_clear_view_with_aggregate from '../bindings/passed/effect_method_asset_clear_view_with_aggregate'
import * as effect_method_asset_clear_view_with_partition from '../bindings/passed/effect_method_asset_clear_view_with_partition'
import * as effect_method_asset_map_0_put_remove_put from '../bindings/passed/effect_method_asset_map_0_put_remove_put'
import * as effect_method_asset_map_0_put_remove_remove from '../bindings/passed/effect_method_asset_map_0_put_remove_remove'
import * as effect_method_asset_map_1_put_remove_put from '../bindings/passed/effect_method_asset_map_1_put_remove_put'
import * as effect_method_asset_map_1_put_remove_remove from '../bindings/passed/effect_method_asset_map_1_put_remove_remove'
import * as effect_method_asset_map_2_put_remove_put from '../bindings/passed/effect_method_asset_map_2_put_remove_put'
import * as effect_method_asset_map_2_put_remove_remove from '../bindings/passed/effect_method_asset_map_2_put_remove_remove'
import * as effect_method_asset_remove_aggregate from '../bindings/passed/effect_method_asset_remove_aggregate'
import * as effect_method_asset_remove_all_aggregate from '../bindings/passed/effect_method_asset_remove_all_aggregate'
import * as effect_method_asset_remove_all_asset_one_field from '../bindings/passed/effect_method_asset_remove_all_asset_one_field'
import * as effect_method_asset_remove_all_asset_with_aggregate from '../bindings/passed/effect_method_asset_remove_all_asset_with_aggregate'
import * as effect_method_asset_remove_all_asset_with_partition from '../bindings/passed/effect_method_asset_remove_all_asset_with_partition'
import * as effect_method_asset_remove_all_collection from '../bindings/passed/effect_method_asset_remove_all_collection'
import * as effect_method_asset_remove_asset from '../bindings/passed/effect_method_asset_remove_asset'
import * as effect_method_asset_remove_asset2 from '../bindings/passed/effect_method_asset_remove_asset2'
import * as effect_method_asset_remove_asset_one_field from '../bindings/passed/effect_method_asset_remove_asset_one_field'
import * as effect_method_asset_remove_asset_with_aggregate from '../bindings/passed/effect_method_asset_remove_asset_with_aggregate'
import * as effect_method_asset_remove_asset_with_partition from '../bindings/passed/effect_method_asset_remove_asset_with_partition'
import * as effect_method_asset_remove_asset_with_partition_2 from '../bindings/passed/effect_method_asset_remove_asset_with_partition_2'
import * as effect_method_asset_remove_partition from '../bindings/passed/effect_method_asset_remove_partition'
import * as effect_method_asset_removeall_aggregate from '../bindings/passed/effect_method_asset_removeall_aggregate'
import * as effect_method_asset_removeall_partition from '../bindings/passed/effect_method_asset_removeall_partition'
import * as effect_method_asset_removeif_aggregate from '../bindings/passed/effect_method_asset_removeif_aggregate'
import * as effect_method_asset_removeif_collection from '../bindings/passed/effect_method_asset_removeif_collection'
import * as effect_method_asset_removeif_collection_with_aggregate from '../bindings/passed/effect_method_asset_removeif_collection_with_aggregate'
import * as effect_method_asset_removeif_collection_with_partition from '../bindings/passed/effect_method_asset_removeif_collection_with_partition'
import * as effect_method_asset_removeif_partition from '../bindings/passed/effect_method_asset_removeif_partition'
import * as effect_method_asset_update from '../bindings/passed/effect_method_asset_update'
import * as effect_method_asset_update_all_coll_1 from '../bindings/passed/effect_method_asset_update_all_coll_1'
import * as effect_method_asset_update_all_coll_2 from '../bindings/passed/effect_method_asset_update_all_coll_2'
import * as effect_method_asset_update_all_view_1 from '../bindings/passed/effect_method_asset_update_all_view_1'
import * as effect_method_asset_update_all_view_2 from '../bindings/passed/effect_method_asset_update_all_view_2'
import * as effect_method_asset_update_with_add_aggregate from '../bindings/passed/effect_method_asset_update_with_add_aggregate'
import * as effect_method_asset_update_with_add_map from '../bindings/passed/effect_method_asset_update_with_add_map'
import * as effect_method_asset_update_with_add_partition from '../bindings/passed/effect_method_asset_update_with_add_partition'
import * as effect_method_asset_update_with_add_set from '../bindings/passed/effect_method_asset_update_with_add_set'
import * as effect_method_asset_update_with_map from '../bindings/passed/effect_method_asset_update_with_map'
import * as effect_method_asset_update_with_remove_aggregate from '../bindings/passed/effect_method_asset_update_with_remove_aggregate'
import * as effect_method_asset_update_with_remove_map from '../bindings/passed/effect_method_asset_update_with_remove_map'
import * as effect_method_asset_update_with_remove_partition from '../bindings/passed/effect_method_asset_update_with_remove_partition'
import * as effect_method_asset_update_with_remove_set from '../bindings/passed/effect_method_asset_update_with_remove_set'
import * as effect_method_asset_update_with_replace_aggregate from '../bindings/passed/effect_method_asset_update_with_replace_aggregate'
import * as effect_method_asset_update_with_replace_partition from '../bindings/passed/effect_method_asset_update_with_replace_partition'
import * as effect_method_asset_update_with_set from '../bindings/passed/effect_method_asset_update_with_set'
import * as effect_transfer_contract from '../bindings/passed/effect_transfer_contract'
import * as effect_transfer_simple from '../bindings/passed/effect_transfer_simple'
import * as entry_inspector from '../bindings/passed/entry_inspector'
import * as entry_section_called_by_otherwise from '../bindings/passed/entry_section_called_by_otherwise'
import * as entry_section_no_transfer_otherwise from '../bindings/passed/entry_section_no_transfer_otherwise'
import * as entry_section_sourced_by_otherwise from '../bindings/passed/entry_section_sourced_by_otherwise'
import * as entry_section_state_is_otherwise from '../bindings/passed/entry_section_state_is_otherwise'
import * as entry_token from '../bindings/passed/entry_token'
import * as entry_without_effect from '../bindings/passed/entry_without_effect'
import * as enum_all from '../bindings/passed/enum_all'
import * as enum_key from '../bindings/passed/enum_key'
import * as enum_with_args from '../bindings/passed/enum_with_args'
import * as enum_with_args_multi from '../bindings/passed/enum_with_args_multi'
import * as enum_without_args from '../bindings/passed/enum_without_args'
import * as event_all from '../bindings/passed/event_all'
import * as event_dup from '../bindings/passed/event_dup'
import * as event_multi from '../bindings/passed/event_multi'
import * as event_simple from '../bindings/passed/event_simple'
import * as event_single from '../bindings/passed/event_single'
import * as exec_letin from '../bindings/passed/exec_letin'
import * as expr_access_asset_field from '../bindings/passed/expr_access_asset_field'
import * as expr_arith_3wc_nat_nat from '../bindings/passed/expr_arith_3wc_nat_nat'
import * as expr_arith_and_bool_bool from '../bindings/passed/expr_arith_and_bool_bool'
import * as expr_arith_and_int_nat from '../bindings/passed/expr_arith_and_int_nat'
import * as expr_arith_and_nat_nat from '../bindings/passed/expr_arith_and_nat_nat'
import * as expr_arith_div_dur_dur from '../bindings/passed/expr_arith_div_dur_dur'
import * as expr_arith_div_int_int from '../bindings/passed/expr_arith_div_int_int'
import * as expr_arith_div_int_nat from '../bindings/passed/expr_arith_div_int_nat'
import * as expr_arith_div_int_rat from '../bindings/passed/expr_arith_div_int_rat'
import * as expr_arith_div_nat_int from '../bindings/passed/expr_arith_div_nat_int'
import * as expr_arith_div_nat_nat from '../bindings/passed/expr_arith_div_nat_nat'
import * as expr_arith_div_nat_rat from '../bindings/passed/expr_arith_div_nat_rat'
import * as expr_arith_div_rat_int from '../bindings/passed/expr_arith_div_rat_int'
import * as expr_arith_div_rat_nat from '../bindings/passed/expr_arith_div_rat_nat'
import * as expr_arith_div_rat_rat from '../bindings/passed/expr_arith_div_rat_rat'
import * as expr_arith_div_tez_tez from '../bindings/passed/expr_arith_div_tez_tez'
import * as expr_arith_divmod_int_int from '../bindings/passed/expr_arith_divmod_int_int'
import * as expr_arith_divmod_int_nat from '../bindings/passed/expr_arith_divmod_int_nat'
import * as expr_arith_divmod_nat_int from '../bindings/passed/expr_arith_divmod_nat_int'
import * as expr_arith_divmod_nat_nat from '../bindings/passed/expr_arith_divmod_nat_nat'
import * as expr_arith_divmod_tez_nat from '../bindings/passed/expr_arith_divmod_tez_nat'
import * as expr_arith_divmod_tez_tez from '../bindings/passed/expr_arith_divmod_tez_tez'
import * as expr_arith_ediv_dur_dur from '../bindings/passed/expr_arith_ediv_dur_dur'
import * as expr_arith_ediv_dur_int from '../bindings/passed/expr_arith_ediv_dur_int'
import * as expr_arith_ediv_dur_nat from '../bindings/passed/expr_arith_ediv_dur_nat'
import * as expr_arith_ediv_int_int from '../bindings/passed/expr_arith_ediv_int_int'
import * as expr_arith_ediv_int_nat from '../bindings/passed/expr_arith_ediv_int_nat'
import * as expr_arith_ediv_nat_int from '../bindings/passed/expr_arith_ediv_nat_int'
import * as expr_arith_ediv_nat_nat from '../bindings/passed/expr_arith_ediv_nat_nat'
import * as expr_arith_ediv_tez_nat from '../bindings/passed/expr_arith_ediv_tez_nat'
import * as expr_arith_ediv_tez_tez from '../bindings/passed/expr_arith_ediv_tez_tez'
import * as expr_arith_lsl_nat_nat from '../bindings/passed/expr_arith_lsl_nat_nat'
import * as expr_arith_lsr_nat_nat from '../bindings/passed/expr_arith_lsr_nat_nat'
import * as expr_arith_minus_date_date from '../bindings/passed/expr_arith_minus_date_date'
import * as expr_arith_minus_date_date_neg from '../bindings/passed/expr_arith_minus_date_date_neg'
import * as expr_arith_minus_date_dur from '../bindings/passed/expr_arith_minus_date_dur'
import * as expr_arith_minus_dur_dur from '../bindings/passed/expr_arith_minus_dur_dur'
import * as expr_arith_minus_int_int from '../bindings/passed/expr_arith_minus_int_int'
import * as expr_arith_minus_int_nat from '../bindings/passed/expr_arith_minus_int_nat'
import * as expr_arith_minus_int_rat from '../bindings/passed/expr_arith_minus_int_rat'
import * as expr_arith_minus_nat_int from '../bindings/passed/expr_arith_minus_nat_int'
import * as expr_arith_minus_nat_nat from '../bindings/passed/expr_arith_minus_nat_nat'
import * as expr_arith_minus_nat_rat from '../bindings/passed/expr_arith_minus_nat_rat'
import * as expr_arith_minus_rat_int from '../bindings/passed/expr_arith_minus_rat_int'
import * as expr_arith_minus_rat_nat from '../bindings/passed/expr_arith_minus_rat_nat'
import * as expr_arith_minus_rat_rat from '../bindings/passed/expr_arith_minus_rat_rat'
import * as expr_arith_minus_tez_tez from '../bindings/passed/expr_arith_minus_tez_tez'
import * as expr_arith_mod_int_int from '../bindings/passed/expr_arith_mod_int_int'
import * as expr_arith_mod_int_nat from '../bindings/passed/expr_arith_mod_int_nat'
import * as expr_arith_mod_nat_int from '../bindings/passed/expr_arith_mod_nat_int'
import * as expr_arith_mod_nat_nat from '../bindings/passed/expr_arith_mod_nat_nat'
import * as expr_arith_mod_tez_tez from '../bindings/passed/expr_arith_mod_tez_tez'
import * as expr_arith_mult_int_dur from '../bindings/passed/expr_arith_mult_int_dur'
import * as expr_arith_mult_int_int from '../bindings/passed/expr_arith_mult_int_int'
import * as expr_arith_mult_int_nat from '../bindings/passed/expr_arith_mult_int_nat'
import * as expr_arith_mult_int_rat from '../bindings/passed/expr_arith_mult_int_rat'
import * as expr_arith_mult_int_tez from '../bindings/passed/expr_arith_mult_int_tez'
import * as expr_arith_mult_nat_dur from '../bindings/passed/expr_arith_mult_nat_dur'
import * as expr_arith_mult_nat_int from '../bindings/passed/expr_arith_mult_nat_int'
import * as expr_arith_mult_nat_nat from '../bindings/passed/expr_arith_mult_nat_nat'
import * as expr_arith_mult_nat_rat from '../bindings/passed/expr_arith_mult_nat_rat'
import * as expr_arith_mult_nat_tez from '../bindings/passed/expr_arith_mult_nat_tez'
import * as expr_arith_mult_rat_dur from '../bindings/passed/expr_arith_mult_rat_dur'
import * as expr_arith_mult_rat_int from '../bindings/passed/expr_arith_mult_rat_int'
import * as expr_arith_mult_rat_nat from '../bindings/passed/expr_arith_mult_rat_nat'
import * as expr_arith_mult_rat_rat from '../bindings/passed/expr_arith_mult_rat_rat'
import * as expr_arith_mult_rat_tez from '../bindings/passed/expr_arith_mult_rat_tez'
import * as expr_arith_mult_tez_nat from '../bindings/passed/expr_arith_mult_tez_nat'
import * as expr_arith_not_bool from '../bindings/passed/expr_arith_not_bool'
import * as expr_arith_not_int from '../bindings/passed/expr_arith_not_int'
import * as expr_arith_not_nat from '../bindings/passed/expr_arith_not_nat'
import * as expr_arith_or_bool_bool from '../bindings/passed/expr_arith_or_bool_bool'
import * as expr_arith_or_nat_nat from '../bindings/passed/expr_arith_or_nat_nat'
import * as expr_arith_plus_date_dur from '../bindings/passed/expr_arith_plus_date_dur'
import * as expr_arith_plus_dur_date from '../bindings/passed/expr_arith_plus_dur_date'
import * as expr_arith_plus_dur_dur from '../bindings/passed/expr_arith_plus_dur_dur'
import * as expr_arith_plus_int_int from '../bindings/passed/expr_arith_plus_int_int'
import * as expr_arith_plus_int_nat from '../bindings/passed/expr_arith_plus_int_nat'
import * as expr_arith_plus_int_rat from '../bindings/passed/expr_arith_plus_int_rat'
import * as expr_arith_plus_nat_int from '../bindings/passed/expr_arith_plus_nat_int'
import * as expr_arith_plus_nat_nat from '../bindings/passed/expr_arith_plus_nat_nat'
import * as expr_arith_plus_nat_rat from '../bindings/passed/expr_arith_plus_nat_rat'
import * as expr_arith_plus_rat_int from '../bindings/passed/expr_arith_plus_rat_int'
import * as expr_arith_plus_rat_nat from '../bindings/passed/expr_arith_plus_rat_nat'
import * as expr_arith_plus_rat_rat from '../bindings/passed/expr_arith_plus_rat_rat'
import * as expr_arith_plus_str_str from '../bindings/passed/expr_arith_plus_str_str'
import * as expr_arith_plus_tez_tez from '../bindings/passed/expr_arith_plus_tez_tez'
import * as expr_arith_uminus_int from '../bindings/passed/expr_arith_uminus_int'
import * as expr_arith_uminus_rat from '../bindings/passed/expr_arith_uminus_rat'
import * as expr_arith_xor_bool_bool from '../bindings/passed/expr_arith_xor_bool_bool'
import * as expr_arith_xor_nat_nat from '../bindings/passed/expr_arith_xor_nat_nat'
import * as expr_cmp_eq_addr_addr from '../bindings/passed/expr_cmp_eq_addr_addr'
import * as expr_cmp_eq_bool_bool from '../bindings/passed/expr_cmp_eq_bool_bool'
import * as expr_cmp_eq_date_date from '../bindings/passed/expr_cmp_eq_date_date'
import * as expr_cmp_eq_dur_dur from '../bindings/passed/expr_cmp_eq_dur_dur'
import * as expr_cmp_eq_int_int from '../bindings/passed/expr_cmp_eq_int_int'
import * as expr_cmp_eq_int_nat from '../bindings/passed/expr_cmp_eq_int_nat'
import * as expr_cmp_eq_int_rat from '../bindings/passed/expr_cmp_eq_int_rat'
import * as expr_cmp_eq_nat_int from '../bindings/passed/expr_cmp_eq_nat_int'
import * as expr_cmp_eq_nat_nat from '../bindings/passed/expr_cmp_eq_nat_nat'
import * as expr_cmp_eq_nat_rat from '../bindings/passed/expr_cmp_eq_nat_rat'
import * as expr_cmp_eq_rat_int from '../bindings/passed/expr_cmp_eq_rat_int'
import * as expr_cmp_eq_rat_nat from '../bindings/passed/expr_cmp_eq_rat_nat'
import * as expr_cmp_eq_rat_rat from '../bindings/passed/expr_cmp_eq_rat_rat'
import * as expr_cmp_eq_str_str from '../bindings/passed/expr_cmp_eq_str_str'
import * as expr_cmp_eq_tez_tez from '../bindings/passed/expr_cmp_eq_tez_tez'
import * as expr_cmp_ge_addr_addr from '../bindings/passed/expr_cmp_ge_addr_addr'
import * as expr_cmp_ge_date_date from '../bindings/passed/expr_cmp_ge_date_date'
import * as expr_cmp_ge_dur_dur from '../bindings/passed/expr_cmp_ge_dur_dur'
import * as expr_cmp_ge_int_int from '../bindings/passed/expr_cmp_ge_int_int'
import * as expr_cmp_ge_int_nat from '../bindings/passed/expr_cmp_ge_int_nat'
import * as expr_cmp_ge_int_rat from '../bindings/passed/expr_cmp_ge_int_rat'
import * as expr_cmp_ge_nat_int from '../bindings/passed/expr_cmp_ge_nat_int'
import * as expr_cmp_ge_nat_nat from '../bindings/passed/expr_cmp_ge_nat_nat'
import * as expr_cmp_ge_nat_rat from '../bindings/passed/expr_cmp_ge_nat_rat'
import * as expr_cmp_ge_rat_int from '../bindings/passed/expr_cmp_ge_rat_int'
import * as expr_cmp_ge_rat_nat from '../bindings/passed/expr_cmp_ge_rat_nat'
import * as expr_cmp_ge_rat_rat from '../bindings/passed/expr_cmp_ge_rat_rat'
import * as expr_cmp_ge_str_str from '../bindings/passed/expr_cmp_ge_str_str'
import * as expr_cmp_ge_tez_tez from '../bindings/passed/expr_cmp_ge_tez_tez'
import * as expr_cmp_gt_addr_addr from '../bindings/passed/expr_cmp_gt_addr_addr'
import * as expr_cmp_gt_date_date from '../bindings/passed/expr_cmp_gt_date_date'
import * as expr_cmp_gt_dur_dur from '../bindings/passed/expr_cmp_gt_dur_dur'
import * as expr_cmp_gt_int_int from '../bindings/passed/expr_cmp_gt_int_int'
import * as expr_cmp_gt_int_nat from '../bindings/passed/expr_cmp_gt_int_nat'
import * as expr_cmp_gt_int_rat from '../bindings/passed/expr_cmp_gt_int_rat'
import * as expr_cmp_gt_nat_int from '../bindings/passed/expr_cmp_gt_nat_int'
import * as expr_cmp_gt_nat_nat from '../bindings/passed/expr_cmp_gt_nat_nat'
import * as expr_cmp_gt_nat_rat from '../bindings/passed/expr_cmp_gt_nat_rat'
import * as expr_cmp_gt_rat_int from '../bindings/passed/expr_cmp_gt_rat_int'
import * as expr_cmp_gt_rat_nat from '../bindings/passed/expr_cmp_gt_rat_nat'
import * as expr_cmp_gt_rat_rat from '../bindings/passed/expr_cmp_gt_rat_rat'
import * as expr_cmp_gt_str_str from '../bindings/passed/expr_cmp_gt_str_str'
import * as expr_cmp_gt_tez_tez from '../bindings/passed/expr_cmp_gt_tez_tez'
import * as expr_cmp_le_addr_addr from '../bindings/passed/expr_cmp_le_addr_addr'
import * as expr_cmp_le_date_date from '../bindings/passed/expr_cmp_le_date_date'
import * as expr_cmp_le_dur_dur from '../bindings/passed/expr_cmp_le_dur_dur'
import * as expr_cmp_le_int_int from '../bindings/passed/expr_cmp_le_int_int'
import * as expr_cmp_le_int_nat from '../bindings/passed/expr_cmp_le_int_nat'
import * as expr_cmp_le_int_rat from '../bindings/passed/expr_cmp_le_int_rat'
import * as expr_cmp_le_nat_int from '../bindings/passed/expr_cmp_le_nat_int'
import * as expr_cmp_le_nat_nat from '../bindings/passed/expr_cmp_le_nat_nat'
import * as expr_cmp_le_nat_rat from '../bindings/passed/expr_cmp_le_nat_rat'
import * as expr_cmp_le_rat_int from '../bindings/passed/expr_cmp_le_rat_int'
import * as expr_cmp_le_rat_nat from '../bindings/passed/expr_cmp_le_rat_nat'
import * as expr_cmp_le_rat_rat from '../bindings/passed/expr_cmp_le_rat_rat'
import * as expr_cmp_le_str_str from '../bindings/passed/expr_cmp_le_str_str'
import * as expr_cmp_le_tez_tez from '../bindings/passed/expr_cmp_le_tez_tez'
import * as expr_cmp_lt_addr_addr from '../bindings/passed/expr_cmp_lt_addr_addr'
import * as expr_cmp_lt_date_date from '../bindings/passed/expr_cmp_lt_date_date'
import * as expr_cmp_lt_dur_dur from '../bindings/passed/expr_cmp_lt_dur_dur'
import * as expr_cmp_lt_int_int from '../bindings/passed/expr_cmp_lt_int_int'
import * as expr_cmp_lt_int_nat from '../bindings/passed/expr_cmp_lt_int_nat'
import * as expr_cmp_lt_int_rat from '../bindings/passed/expr_cmp_lt_int_rat'
import * as expr_cmp_lt_nat_int from '../bindings/passed/expr_cmp_lt_nat_int'
import * as expr_cmp_lt_nat_nat from '../bindings/passed/expr_cmp_lt_nat_nat'
import * as expr_cmp_lt_nat_rat from '../bindings/passed/expr_cmp_lt_nat_rat'
import * as expr_cmp_lt_rat_int from '../bindings/passed/expr_cmp_lt_rat_int'
import * as expr_cmp_lt_rat_nat from '../bindings/passed/expr_cmp_lt_rat_nat'
import * as expr_cmp_lt_rat_rat from '../bindings/passed/expr_cmp_lt_rat_rat'
import * as expr_cmp_lt_str_str from '../bindings/passed/expr_cmp_lt_str_str'
import * as expr_cmp_lt_tez_tez from '../bindings/passed/expr_cmp_lt_tez_tez'
import * as expr_cmp_ne_addr_addr from '../bindings/passed/expr_cmp_ne_addr_addr'
import * as expr_cmp_ne_bool_bool from '../bindings/passed/expr_cmp_ne_bool_bool'
import * as expr_cmp_ne_date_date from '../bindings/passed/expr_cmp_ne_date_date'
import * as expr_cmp_ne_dur_dur from '../bindings/passed/expr_cmp_ne_dur_dur'
import * as expr_cmp_ne_int_int from '../bindings/passed/expr_cmp_ne_int_int'
import * as expr_cmp_ne_int_nat from '../bindings/passed/expr_cmp_ne_int_nat'
import * as expr_cmp_ne_int_rat from '../bindings/passed/expr_cmp_ne_int_rat'
import * as expr_cmp_ne_nat_int from '../bindings/passed/expr_cmp_ne_nat_int'
import * as expr_cmp_ne_nat_nat from '../bindings/passed/expr_cmp_ne_nat_nat'
import * as expr_cmp_ne_nat_rat from '../bindings/passed/expr_cmp_ne_nat_rat'
import * as expr_cmp_ne_rat_int from '../bindings/passed/expr_cmp_ne_rat_int'
import * as expr_cmp_ne_rat_nat from '../bindings/passed/expr_cmp_ne_rat_nat'
import * as expr_cmp_ne_rat_rat from '../bindings/passed/expr_cmp_ne_rat_rat'
import * as expr_cmp_ne_str_str from '../bindings/passed/expr_cmp_ne_str_str'
import * as expr_cmp_ne_tez_tez from '../bindings/passed/expr_cmp_ne_tez_tez'
import * as expr_control_fold from '../bindings/passed/expr_control_fold'
import * as expr_control_if_else_int_int from '../bindings/passed/expr_control_if_else_int_int'
import * as expr_control_if_else_int_nat from '../bindings/passed/expr_control_if_else_int_nat'
import * as expr_control_if_else_int_rat from '../bindings/passed/expr_control_if_else_int_rat'
import * as expr_control_if_else_nat_int from '../bindings/passed/expr_control_if_else_nat_int'
import * as expr_control_if_else_nat_nat from '../bindings/passed/expr_control_if_else_nat_nat'
import * as expr_control_if_else_nat_rat from '../bindings/passed/expr_control_if_else_nat_rat'
import * as expr_control_if_else_rat_int from '../bindings/passed/expr_control_if_else_rat_int'
import * as expr_control_if_else_rat_nat from '../bindings/passed/expr_control_if_else_rat_nat'
import * as expr_control_if_else_rat_rat from '../bindings/passed/expr_control_if_else_rat_rat'
import * as expr_control_match_list from '../bindings/passed/expr_control_match_list'
import * as expr_control_match_option from '../bindings/passed/expr_control_match_option'
import * as expr_control_match_or from '../bindings/passed/expr_control_match_or'
import * as expr_control_matchwith from '../bindings/passed/expr_control_matchwith'
import * as expr_control_matchwith_with_int_rat from '../bindings/passed/expr_control_matchwith_with_int_rat'
import * as expr_control_matchwith_with_nat_int from '../bindings/passed/expr_control_matchwith_with_nat_int'
import * as expr_control_matchwith_with_nat_rat from '../bindings/passed/expr_control_matchwith_with_nat_rat'
import * as expr_cst_balance from '../bindings/passed/expr_cst_balance'
import * as expr_cst_caller from '../bindings/passed/expr_cst_caller'
import * as expr_cst_level from '../bindings/passed/expr_cst_level'
import * as expr_cst_min_block_time from '../bindings/passed/expr_cst_min_block_time'
import * as expr_cst_now from '../bindings/passed/expr_cst_now'
import * as expr_cst_self_address from '../bindings/passed/expr_cst_self_address'
import * as expr_cst_self_chain_id from '../bindings/passed/expr_cst_self_chain_id'
import * as expr_cst_source from '../bindings/passed/expr_cst_source'
import * as expr_cst_total_voting_power from '../bindings/passed/expr_cst_total_voting_power'
import * as expr_cst_transferred from '../bindings/passed/expr_cst_transferred'
import * as expr_fail_some_none from '../bindings/passed/expr_fail_some_none'
import * as expr_fail_some_some from '../bindings/passed/expr_fail_some_some'
import * as expr_formula_asset_method_contains from '../bindings/passed/expr_formula_asset_method_contains'
import * as expr_formula_asset_method_count from '../bindings/passed/expr_formula_asset_method_count'
import * as expr_formula_asset_method_diff_view from '../bindings/passed/expr_formula_asset_method_diff_view'
import * as expr_formula_asset_method_empty from '../bindings/passed/expr_formula_asset_method_empty'
import * as expr_formula_asset_method_get from '../bindings/passed/expr_formula_asset_method_get'
import * as expr_formula_asset_method_head from '../bindings/passed/expr_formula_asset_method_head'
import * as expr_formula_asset_method_inter_view from '../bindings/passed/expr_formula_asset_method_inter_view'
import * as expr_formula_asset_method_isempty from '../bindings/passed/expr_formula_asset_method_isempty'
import * as expr_formula_asset_method_nth from '../bindings/passed/expr_formula_asset_method_nth'
import * as expr_formula_asset_method_select from '../bindings/passed/expr_formula_asset_method_select'
import * as expr_formula_asset_method_singleton from '../bindings/passed/expr_formula_asset_method_singleton'
import * as expr_formula_asset_method_subsetof_aggregate from '../bindings/passed/expr_formula_asset_method_subsetof_aggregate'
import * as expr_formula_asset_method_subsetof_collection from '../bindings/passed/expr_formula_asset_method_subsetof_collection'
import * as expr_formula_asset_method_subsetof_partition from '../bindings/passed/expr_formula_asset_method_subsetof_partition'
import * as expr_formula_asset_method_subsetof_view from '../bindings/passed/expr_formula_asset_method_subsetof_view'
import * as expr_formula_asset_method_sum from '../bindings/passed/expr_formula_asset_method_sum'
import * as expr_formula_asset_method_tail from '../bindings/passed/expr_formula_asset_method_tail'
import * as expr_formula_asset_method_union_view from '../bindings/passed/expr_formula_asset_method_union_view'
import * as expr_formula_at from '../bindings/passed/expr_formula_at'
import * as expr_formula_before from '../bindings/passed/expr_formula_before'
import * as expr_formula_cmp_eq_list from '../bindings/passed/expr_formula_cmp_eq_list'
import * as expr_formula_cmp_eq_map from '../bindings/passed/expr_formula_cmp_eq_map'
import * as expr_formula_cmp_eq_option from '../bindings/passed/expr_formula_cmp_eq_option'
import * as expr_formula_cmp_eq_set from '../bindings/passed/expr_formula_cmp_eq_set'
import * as expr_formula_cmp_ne_list from '../bindings/passed/expr_formula_cmp_ne_list'
import * as expr_formula_cmp_ne_map from '../bindings/passed/expr_formula_cmp_ne_map'
import * as expr_formula_cmp_ne_option from '../bindings/passed/expr_formula_cmp_ne_option'
import * as expr_formula_cmp_ne_set from '../bindings/passed/expr_formula_cmp_ne_set'
import * as expr_formula_equiv from '../bindings/passed/expr_formula_equiv'
import * as expr_formula_exists_asset from '../bindings/passed/expr_formula_exists_asset'
import * as expr_formula_exists_builtin from '../bindings/passed/expr_formula_exists_builtin'
import * as expr_formula_forall_asset from '../bindings/passed/expr_formula_forall_asset'
import * as expr_formula_forall_builtin from '../bindings/passed/expr_formula_forall_builtin'
import * as expr_formula_implication from '../bindings/passed/expr_formula_implication'
import * as expr_formula_iterated_aggregate from '../bindings/passed/expr_formula_iterated_aggregate'
import * as expr_formula_iterated_collection from '../bindings/passed/expr_formula_iterated_collection'
import * as expr_formula_iterated_partition from '../bindings/passed/expr_formula_iterated_partition'
import * as expr_formula_iterated_view from '../bindings/passed/expr_formula_iterated_view'
import * as expr_formula_toiterate_aggregate from '../bindings/passed/expr_formula_toiterate_aggregate'
import * as expr_formula_toiterate_collection from '../bindings/passed/expr_formula_toiterate_collection'
import * as expr_formula_toiterate_partition from '../bindings/passed/expr_formula_toiterate_partition'
import * as expr_formula_toiterate_view from '../bindings/passed/expr_formula_toiterate_view'
import * as expr_fun_abs_int from '../bindings/passed/expr_fun_abs_int'
import * as expr_fun_abs_rat from '../bindings/passed/expr_fun_abs_rat'
import * as expr_fun_address_to_contract from '../bindings/passed/expr_fun_address_to_contract'
import * as expr_fun_address_to_contract_unit from '../bindings/passed/expr_fun_address_to_contract_unit'
import * as expr_fun_ceil from '../bindings/passed/expr_fun_ceil'
import * as expr_fun_concat_byt from '../bindings/passed/expr_fun_concat_byt'
import * as expr_fun_concat_list_byt from '../bindings/passed/expr_fun_concat_list_byt'
import * as expr_fun_concat_list_str from '../bindings/passed/expr_fun_concat_list_str'
import * as expr_fun_concat_str from '../bindings/passed/expr_fun_concat_str'
import * as expr_fun_floor from '../bindings/passed/expr_fun_floor'
import * as expr_fun_int_to_nat from '../bindings/passed/expr_fun_int_to_nat'
import * as expr_fun_key_hash_to_contract from '../bindings/passed/expr_fun_key_hash_to_contract'
import * as expr_fun_length_bytes from '../bindings/passed/expr_fun_length_bytes'
import * as expr_fun_length_str from '../bindings/passed/expr_fun_length_str'
import * as expr_fun_make_event from '../bindings/passed/expr_fun_make_event'
import * as expr_fun_make_operation from '../bindings/passed/expr_fun_make_operation'
import * as expr_fun_max_date from '../bindings/passed/expr_fun_max_date'
import * as expr_fun_max_dur from '../bindings/passed/expr_fun_max_dur'
import * as expr_fun_max_int_int from '../bindings/passed/expr_fun_max_int_int'
import * as expr_fun_max_int_nat from '../bindings/passed/expr_fun_max_int_nat'
import * as expr_fun_max_int_rat from '../bindings/passed/expr_fun_max_int_rat'
import * as expr_fun_max_nat_int from '../bindings/passed/expr_fun_max_nat_int'
import * as expr_fun_max_nat_nat from '../bindings/passed/expr_fun_max_nat_nat'
import * as expr_fun_max_nat_rat from '../bindings/passed/expr_fun_max_nat_rat'
import * as expr_fun_max_rat_int from '../bindings/passed/expr_fun_max_rat_int'
import * as expr_fun_max_rat_nat from '../bindings/passed/expr_fun_max_rat_nat'
import * as expr_fun_max_rat_rat from '../bindings/passed/expr_fun_max_rat_rat'
import * as expr_fun_max_tez from '../bindings/passed/expr_fun_max_tez'
import * as expr_fun_min_date from '../bindings/passed/expr_fun_min_date'
import * as expr_fun_min_dur from '../bindings/passed/expr_fun_min_dur'
import * as expr_fun_min_int_int from '../bindings/passed/expr_fun_min_int_int'
import * as expr_fun_min_int_nat from '../bindings/passed/expr_fun_min_int_nat'
import * as expr_fun_min_int_rat from '../bindings/passed/expr_fun_min_int_rat'
import * as expr_fun_min_nat_int from '../bindings/passed/expr_fun_min_nat_int'
import * as expr_fun_min_nat_nat from '../bindings/passed/expr_fun_min_nat_nat'
import * as expr_fun_min_nat_rat from '../bindings/passed/expr_fun_min_nat_rat'
import * as expr_fun_min_rat_int from '../bindings/passed/expr_fun_min_rat_int'
import * as expr_fun_min_rat_nat from '../bindings/passed/expr_fun_min_rat_nat'
import * as expr_fun_min_rat_rat from '../bindings/passed/expr_fun_min_rat_rat'
import * as expr_fun_min_tez from '../bindings/passed/expr_fun_min_tez'
import * as expr_fun_nat_to_string from '../bindings/passed/expr_fun_nat_to_string'
import * as expr_fun_opt_get_some from '../bindings/passed/expr_fun_opt_get_some'
import * as expr_fun_opt_is_none from '../bindings/passed/expr_fun_opt_is_none'
import * as expr_fun_opt_is_some from '../bindings/passed/expr_fun_opt_is_some'
import * as expr_fun_opt_require_some from '../bindings/passed/expr_fun_opt_require_some'
import * as expr_fun_pack_complex from '../bindings/passed/expr_fun_pack_complex'
import * as expr_fun_pack_lit_tuple from '../bindings/passed/expr_fun_pack_lit_tuple'
import * as expr_fun_pack_string from '../bindings/passed/expr_fun_pack_string'
import * as expr_fun_setdelegate from '../bindings/passed/expr_fun_setdelegate'
import * as expr_fun_slice_byt from '../bindings/passed/expr_fun_slice_byt'
import * as expr_fun_slice_str from '../bindings/passed/expr_fun_slice_str'
import * as expr_fun_sub_mutez from '../bindings/passed/expr_fun_sub_mutez'
import * as expr_fun_sub_nat from '../bindings/passed/expr_fun_sub_nat'
import * as expr_fun_sub_nat_zero from '../bindings/passed/expr_fun_sub_nat_zero'
import * as expr_fun_unpack_bool from '../bindings/passed/expr_fun_unpack_bool'
import * as expr_fun_unpack_complex from '../bindings/passed/expr_fun_unpack_complex'
import * as expr_fun_unpack_string from '../bindings/passed/expr_fun_unpack_string'
import * as expr_instr_rec_1_0 from '../bindings/passed/expr_instr_rec_1_0'
import * as expr_instr_rec_2_0 from '../bindings/passed/expr_instr_rec_2_0'
import * as expr_instr_rec_2_1 from '../bindings/passed/expr_instr_rec_2_1'
import * as expr_instr_rec_3_0 from '../bindings/passed/expr_instr_rec_3_0'
import * as expr_instr_rec_3_1 from '../bindings/passed/expr_instr_rec_3_1'
import * as expr_instr_rec_3_2 from '../bindings/passed/expr_instr_rec_3_2'
import * as expr_instr_rec_4_0 from '../bindings/passed/expr_instr_rec_4_0'
import * as expr_instr_rec_4_1 from '../bindings/passed/expr_instr_rec_4_1'
import * as expr_instr_rec_4_2 from '../bindings/passed/expr_instr_rec_4_2'
import * as expr_instr_rec_4_3 from '../bindings/passed/expr_instr_rec_4_3'
import * as expr_instr_rec_rollback from '../bindings/passed/expr_instr_rec_rollback'
import * as expr_lambda from '../bindings/passed/expr_lambda'
import * as expr_lambda2 from '../bindings/passed/expr_lambda2'
import * as expr_list_concat from '../bindings/passed/expr_list_concat'
import * as expr_list_contains from '../bindings/passed/expr_list_contains'
import * as expr_list_length from '../bindings/passed/expr_list_length'
import * as expr_list_lit from '../bindings/passed/expr_list_lit'
import * as expr_list_map from '../bindings/passed/expr_list_map'
import * as expr_list_map_string_nat from '../bindings/passed/expr_list_map_string_nat'
import * as expr_list_nth from '../bindings/passed/expr_list_nth'
import * as expr_list_prepend from '../bindings/passed/expr_list_prepend'
import * as expr_list_reverse from '../bindings/passed/expr_list_reverse'
import * as expr_lit_addr from '../bindings/passed/expr_lit_addr'
import * as expr_lit_bytes from '../bindings/passed/expr_lit_bytes'
import * as expr_lit_cur_mtz from '../bindings/passed/expr_lit_cur_mtz'
import * as expr_lit_cur_tz from '../bindings/passed/expr_lit_cur_tz'
import * as expr_lit_cur_utz from '../bindings/passed/expr_lit_cur_utz'
import * as expr_lit_date_0 from '../bindings/passed/expr_lit_date_0'
import * as expr_lit_date_1 from '../bindings/passed/expr_lit_date_1'
import * as expr_lit_date_2 from '../bindings/passed/expr_lit_date_2'
import * as expr_lit_date_3 from '../bindings/passed/expr_lit_date_3'
import * as expr_lit_date_4 from '../bindings/passed/expr_lit_date_4'
import * as expr_lit_dur from '../bindings/passed/expr_lit_dur'
import * as expr_lit_int from '../bindings/passed/expr_lit_int'
import * as expr_lit_int_neg from '../bindings/passed/expr_lit_int_neg'
import * as expr_lit_nat from '../bindings/passed/expr_lit_nat'
import * as expr_lit_opt_none from '../bindings/passed/expr_lit_opt_none'
import * as expr_lit_opt_some from '../bindings/passed/expr_lit_opt_some'
import * as expr_lit_or_left from '../bindings/passed/expr_lit_or_left'
import * as expr_lit_or_right from '../bindings/passed/expr_lit_or_right'
import * as expr_lit_rat_dec from '../bindings/passed/expr_lit_rat_dec'
import * as expr_lit_rat_dec_neg from '../bindings/passed/expr_lit_rat_dec_neg'
import * as expr_lit_rat_div from '../bindings/passed/expr_lit_rat_div'
import * as expr_lit_rat_div_neg from '../bindings/passed/expr_lit_rat_div_neg'
import * as expr_lit_str from '../bindings/passed/expr_lit_str'
import * as expr_lit_tuple from '../bindings/passed/expr_lit_tuple'
import * as expr_lit_unit from '../bindings/passed/expr_lit_unit'
import * as expr_make_big_map from '../bindings/passed/expr_make_big_map'
import * as expr_make_big_map_empty from '../bindings/passed/expr_make_big_map_empty'
import * as expr_make_list from '../bindings/passed/expr_make_list'
import * as expr_make_list_empty from '../bindings/passed/expr_make_list_empty'
import * as expr_make_map from '../bindings/passed/expr_make_map'
import * as expr_make_map_empty from '../bindings/passed/expr_make_map_empty'
import * as expr_make_set from '../bindings/passed/expr_make_set'
import * as expr_make_set_empty from '../bindings/passed/expr_make_set_empty'
import * as expr_map_contains from '../bindings/passed/expr_map_contains'
import * as expr_map_get from '../bindings/passed/expr_map_get'
import * as expr_map_length from '../bindings/passed/expr_map_length'
import * as expr_map_lit from '../bindings/passed/expr_map_lit'
import * as expr_map_map from '../bindings/passed/expr_map_map'
import * as expr_map_put from '../bindings/passed/expr_map_put'
import * as expr_map_remove from '../bindings/passed/expr_map_remove'
import * as expr_map_update from '../bindings/passed/expr_map_update'
import * as expr_method_asset_contains from '../bindings/passed/expr_method_asset_contains'
import * as expr_method_asset_contains_aggregate from '../bindings/passed/expr_method_asset_contains_aggregate'
import * as expr_method_asset_contains_one_field from '../bindings/passed/expr_method_asset_contains_one_field'
import * as expr_method_asset_contains_partition from '../bindings/passed/expr_method_asset_contains_partition'
import * as expr_method_asset_contains_view from '../bindings/passed/expr_method_asset_contains_view'
import * as expr_method_asset_count from '../bindings/passed/expr_method_asset_count'
import * as expr_method_asset_count_aggregate from '../bindings/passed/expr_method_asset_count_aggregate'
import * as expr_method_asset_count_one_field from '../bindings/passed/expr_method_asset_count_one_field'
import * as expr_method_asset_count_partition from '../bindings/passed/expr_method_asset_count_partition'
import * as expr_method_asset_count_view from '../bindings/passed/expr_method_asset_count_view'
import * as expr_method_asset_get from '../bindings/passed/expr_method_asset_get'
import * as expr_method_asset_head from '../bindings/passed/expr_method_asset_head'
import * as expr_method_asset_head_aggregate from '../bindings/passed/expr_method_asset_head_aggregate'
import * as expr_method_asset_head_one_field from '../bindings/passed/expr_method_asset_head_one_field'
import * as expr_method_asset_head_partition from '../bindings/passed/expr_method_asset_head_partition'
import * as expr_method_asset_head_view from '../bindings/passed/expr_method_asset_head_view'
import * as expr_method_asset_nth from '../bindings/passed/expr_method_asset_nth'
import * as expr_method_asset_nth_aggregate from '../bindings/passed/expr_method_asset_nth_aggregate'
import * as expr_method_asset_nth_one_field from '../bindings/passed/expr_method_asset_nth_one_field'
import * as expr_method_asset_nth_partition from '../bindings/passed/expr_method_asset_nth_partition'
import * as expr_method_asset_nth_view from '../bindings/passed/expr_method_asset_nth_view'
import * as expr_method_asset_select from '../bindings/passed/expr_method_asset_select'
import * as expr_method_asset_select_aggregate from '../bindings/passed/expr_method_asset_select_aggregate'
import * as expr_method_asset_select_one_field from '../bindings/passed/expr_method_asset_select_one_field'
import * as expr_method_asset_select_partition from '../bindings/passed/expr_method_asset_select_partition'
import * as expr_method_asset_select_view from '../bindings/passed/expr_method_asset_select_view'
import * as expr_method_asset_sort from '../bindings/passed/expr_method_asset_sort'
import * as expr_method_asset_sort_aggregate from '../bindings/passed/expr_method_asset_sort_aggregate'
import * as expr_method_asset_sort_one_field from '../bindings/passed/expr_method_asset_sort_one_field'
import * as expr_method_asset_sort_partition from '../bindings/passed/expr_method_asset_sort_partition'
import * as expr_method_asset_sort_view from '../bindings/passed/expr_method_asset_sort_view'
import * as expr_method_asset_sum from '../bindings/passed/expr_method_asset_sum'
import * as expr_method_asset_sum_aggregate from '../bindings/passed/expr_method_asset_sum_aggregate'
import * as expr_method_asset_sum_one_field from '../bindings/passed/expr_method_asset_sum_one_field'
import * as expr_method_asset_sum_partition from '../bindings/passed/expr_method_asset_sum_partition'
import * as expr_method_asset_sum_rational from '../bindings/passed/expr_method_asset_sum_rational'
import * as expr_method_asset_sum_view from '../bindings/passed/expr_method_asset_sum_view'
import * as expr_method_asset_tail from '../bindings/passed/expr_method_asset_tail'
import * as expr_method_asset_tail_aggregate from '../bindings/passed/expr_method_asset_tail_aggregate'
import * as expr_method_asset_tail_one_field from '../bindings/passed/expr_method_asset_tail_one_field'
import * as expr_method_asset_tail_partition from '../bindings/passed/expr_method_asset_tail_partition'
import * as expr_method_asset_tail_view from '../bindings/passed/expr_method_asset_tail_view'
import * as expr_multicmp from '../bindings/passed/expr_multicmp'
import * as expr_option_map from '../bindings/passed/expr_option_map'
import * as expr_record_lit from '../bindings/passed/expr_record_lit'
import * as expr_record_update_asset_in_formula from '../bindings/passed/expr_record_update_asset_in_formula'
import * as expr_record_update_record_in_exec from '../bindings/passed/expr_record_update_record_in_exec'
import * as expr_record_update_record_in_formula from '../bindings/passed/expr_record_update_record_in_formula'
import * as expr_set_add from '../bindings/passed/expr_set_add'
import * as expr_set_contains from '../bindings/passed/expr_set_contains'
import * as expr_set_length from '../bindings/passed/expr_set_length'
import * as expr_set_lit from '../bindings/passed/expr_set_lit'
import * as expr_set_remove from '../bindings/passed/expr_set_remove'
import * as expr_set_update from '../bindings/passed/expr_set_update'
import * as expr_tuple_access from '../bindings/passed/expr_tuple_access'
import * as expr_tuple_access_simple from '../bindings/passed/expr_tuple_access_simple'
import * as fa12_false from '../bindings/passed/fa12_false'
import * as fa12_simple from '../bindings/passed/fa12_simple'
import * as fail_ from '../bindings/passed/fail_'
import * as fail_for from '../bindings/passed/fail_for'
import * as fail_if from '../bindings/passed/fail_if'
import * as fail_match_list from '../bindings/passed/fail_match_list'
import * as fail_match_option from '../bindings/passed/fail_match_option'
import * as fail_while from '../bindings/passed/fail_while'
import * as fold_reverse from '../bindings/passed/fold_reverse'
import * as formula_added_asset from '../bindings/passed/formula_added_asset'
import * as fun from '../bindings/passed/fun'
import * as get_in_require_failif from '../bindings/passed/get_in_require_failif'
import * as get_some_with_msg from '../bindings/passed/get_some_with_msg'
import * as greedy_and from '../bindings/passed/greedy_and'
import * as greedy_or from '../bindings/passed/greedy_or'
import * as implicit_cast_to_view from '../bindings/passed/implicit_cast_to_view'
import * as implicit_the from '../bindings/passed/implicit_the'
import * as init_lambda from '../bindings/passed/init_lambda'
import * as instr_list_prepend from '../bindings/passed/instr_list_prepend'
import * as instr_map_put from '../bindings/passed/instr_map_put'
import * as instr_map_remove from '../bindings/passed/instr_map_remove'
import * as instr_map_update_local_record from '../bindings/passed/instr_map_update_local_record'
import * as instr_map_update_local_var from '../bindings/passed/instr_map_update_local_var'
import * as instr_map_update_storage_record from '../bindings/passed/instr_map_update_storage_record'
import * as instr_map_update_storage_var from '../bindings/passed/instr_map_update_storage_var'
import * as instr_set_add from '../bindings/passed/instr_set_add'
import * as instr_set_remove from '../bindings/passed/instr_set_remove'
import * as instr_set_update_add from '../bindings/passed/instr_set_update_add'
import * as instr_set_update_remove from '../bindings/passed/instr_set_update_remove'
import * as int_to_date from '../bindings/passed/int_to_date'
import * as invariants_on_states from '../bindings/passed/invariants_on_states'
import * as invariants_on_variable from '../bindings/passed/invariants_on_variable'
import * as iterable_big_map_assign from '../bindings/passed/iterable_big_map_assign'
import * as iterable_big_map_contains from '../bindings/passed/iterable_big_map_contains'
import * as iterable_big_map_for from '../bindings/passed/iterable_big_map_for'
import * as iterable_big_map_get from '../bindings/passed/iterable_big_map_get'
import * as iterable_big_map_length from '../bindings/passed/iterable_big_map_length'
import * as iterable_big_map_put from '../bindings/passed/iterable_big_map_put'
import * as iterable_big_map_remove from '../bindings/passed/iterable_big_map_remove'
import * as iterable_big_map_storage_decl from '../bindings/passed/iterable_big_map_storage_decl'
import * as iterable_big_map_test from '../bindings/passed/iterable_big_map_test'
import * as key_to_address from '../bindings/passed/key_to_address'
import * as lang_arith from '../bindings/passed/lang_arith'
import * as lang_asset from '../bindings/passed/lang_asset'
import * as lang_assign from '../bindings/passed/lang_assign'
import * as lang_big_map from '../bindings/passed/lang_big_map'
import * as lang_cast from '../bindings/passed/lang_cast'
import * as lang_cmp from '../bindings/passed/lang_cmp'
import * as lang_contract from '../bindings/passed/lang_contract'
import * as lang_crypto from '../bindings/passed/lang_crypto'
import * as lang_cst from '../bindings/passed/lang_cst'
import * as lang_entry from '../bindings/passed/lang_entry'
import * as lang_formula_asset_api from '../bindings/passed/lang_formula_asset_api'
import * as lang_funs from '../bindings/passed/lang_funs'
import * as lang_list from '../bindings/passed/lang_list'
import * as lang_literals from '../bindings/passed/lang_literals'
import * as lang_map from '../bindings/passed/lang_map'
import * as lang_methods_asset from '../bindings/passed/lang_methods_asset'
import * as lang_security from '../bindings/passed/lang_security'
import * as lang_set from '../bindings/passed/lang_set'
import * as list_list from '../bindings/passed/list_list'
import * as list_nth_out_of_bound from '../bindings/passed/list_nth_out_of_bound'
import * as list_option from '../bindings/passed/list_option'
import * as list_or from '../bindings/passed/list_or'
import * as lit_tez_underscore from '../bindings/passed/lit_tez_underscore'
import * as literal_in_argument from '../bindings/passed/literal_in_argument'
import * as map_asset from '../bindings/passed/map_asset'
import * as match_entrypoint from '../bindings/passed/match_entrypoint'
import * as max_tez from '../bindings/passed/max_tez'
import * as method_in_dorequire_or_dofailif from '../bindings/passed/method_in_dorequire_or_dofailif'
import * as miles_with_expiration_spec from '../bindings/passed/miles_with_expiration_spec'
import * as multi_e from '../bindings/passed/multi_e'
import * as multi_p from '../bindings/passed/multi_p'
import * as multi_sort from '../bindings/passed/multi_sort'
import * as multi_update from '../bindings/passed/multi_update'
import * as multi_var_storage from '../bindings/passed/multi_var_storage'
import * as multivars from '../bindings/passed/multivars'
import * as multivars1 from '../bindings/passed/multivars1'
import * as multivars_simple from '../bindings/passed/multivars_simple'
import * as mutez_to_nat from '../bindings/passed/mutez_to_nat'
import * as nat_to_string from '../bindings/passed/nat_to_string'
import * as nested_for from '../bindings/passed/nested_for'
import * as nested_if_return from '../bindings/passed/nested_if_return'
import * as no_entrypoint from '../bindings/passed/no_entrypoint'
import * as not_int from '../bindings/passed/not_int'
import * as not_nat from '../bindings/passed/not_nat'
import * as nothing from '../bindings/passed/nothing'
import * as one_constant from '../bindings/passed/one_constant'
import * as op_assign_rat_update_asset from '../bindings/passed/op_assign_rat_update_asset'
import * as parameter_expr_map from '../bindings/passed/parameter_expr_map'
import * as partial_record from '../bindings/passed/partial_record'
import * as rat_arith_div from '../bindings/passed/rat_arith_div'
import * as rat_arith_minus from '../bindings/passed/rat_arith_minus'
import * as rat_arith_mult from '../bindings/passed/rat_arith_mult'
import * as rat_arith_plus from '../bindings/passed/rat_arith_plus'
import * as rat_arith_uminus from '../bindings/passed/rat_arith_uminus'
import * as rat_cmp_eq from '../bindings/passed/rat_cmp_eq'
import * as rat_cmp_ge from '../bindings/passed/rat_cmp_ge'
import * as rat_cmp_gt from '../bindings/passed/rat_cmp_gt'
import * as rat_cmp_le from '../bindings/passed/rat_cmp_le'
import * as rat_cmp_lt from '../bindings/passed/rat_cmp_lt'
import * as rat_dur from '../bindings/passed/rat_dur'
import * as rat_int from '../bindings/passed/rat_int'
import * as rat_max from '../bindings/passed/rat_max'
import * as rat_min from '../bindings/passed/rat_min'
import * as rat_nat from '../bindings/passed/rat_nat'
import * as rat_neg from '../bindings/passed/rat_neg'
import * as rat_tez from '../bindings/passed/rat_tez'
import * as rat_tez_big from '../bindings/passed/rat_tez_big'
import * as rational_cmp from '../bindings/passed/rational_cmp'
import * as rational_duration from '../bindings/passed/rational_duration'
import * as rational_full from '../bindings/passed/rational_full'
import * as rational_in_formula from '../bindings/passed/rational_in_formula'
import * as rational_rat_tez_mult from '../bindings/passed/rational_rat_tez_mult'
import * as rational_simple from '../bindings/passed/rational_simple'
import * as rational_tez_rat_mult from '../bindings/passed/rational_tez_rat_mult'
import * as rec_update from '../bindings/passed/rec_update'
import * as rec_update2 from '../bindings/passed/rec_update2'
import * as record_container from '../bindings/passed/record_container'
import * as record_double_key from '../bindings/passed/record_double_key'
import * as record_in_enum from '../bindings/passed/record_in_enum'
import * as record_update from '../bindings/passed/record_update'
import * as remove_asset_with_partition from '../bindings/passed/remove_asset_with_partition'
import * as reverse_otherwise from '../bindings/passed/reverse_otherwise'
import * as reverse_with_enum from '../bindings/passed/reverse_with_enum'
import * as rf_require_otherwise from '../bindings/passed/rf_require_otherwise'
import * as same_varname_in_two_distinct_scope from '../bindings/passed/same_varname_in_two_distinct_scope'
import * as sample_asset_view from '../bindings/passed/sample_asset_view'
import * as sapling_empty_state from '../bindings/passed/sapling_empty_state'
import * as sapling_var from '../bindings/passed/sapling_var'
import * as sapling_verify_update from '../bindings/passed/sapling_verify_update'
import * as section_constant_effect from '../bindings/passed/section_constant_effect'
import * as section_constant_transition from '../bindings/passed/section_constant_transition'
import * as security_pred_no_storage_fail from '../bindings/passed/security_pred_no_storage_fail'
import * as security_pred_not_by_role from '../bindings/passed/security_pred_not_by_role'
import * as security_pred_not_by_role_in_entry from '../bindings/passed/security_pred_not_by_role_in_entry'
import * as security_pred_not_in_entry from '../bindings/passed/security_pred_not_in_entry'
import * as security_pred_only_by_role from '../bindings/passed/security_pred_only_by_role'
import * as security_pred_only_by_role_in_entry from '../bindings/passed/security_pred_only_by_role_in_entry'
import * as security_pred_only_in_entry from '../bindings/passed/security_pred_only_in_entry'
import * as security_pred_transferred_by from '../bindings/passed/security_pred_transferred_by'
import * as security_pred_transferred_to from '../bindings/passed/security_pred_transferred_to'
import * as select_partition from '../bindings/passed/select_partition'
import * as select_partition_big_map from '../bindings/passed/select_partition_big_map'
import * as select_with_extra_var from '../bindings/passed/select_with_extra_var'
import * as select_with_extra_var2 from '../bindings/passed/select_with_extra_var2'
import * as select_with_function_in_predicate from '../bindings/passed/select_with_function_in_predicate'
import * as setdelegate from '../bindings/passed/setdelegate'
import * as shadow_field from '../bindings/passed/shadow_field'
import * as shadow_global_var_effect from '../bindings/passed/shadow_global_var_effect'
import * as shadow_var from '../bindings/passed/shadow_var'
import * as shadow_var_scope from '../bindings/passed/shadow_var_scope'
import * as simple from '../bindings/passed/simple'
import * as simple2 from '../bindings/passed/simple2'
import * as simple3 from '../bindings/passed/simple3'
import * as simple4 from '../bindings/passed/simple4'
import * as simple_2vars from '../bindings/passed/simple_2vars'
import * as simple_addupdate from '../bindings/passed/simple_addupdate'
import * as simple_addupdate_asset from '../bindings/passed/simple_addupdate_asset'
import * as simple_arg_int from '../bindings/passed/simple_arg_int'
import * as simple_arith from '../bindings/passed/simple_arith'
import * as simple_asset from '../bindings/passed/simple_asset'
import * as simple_asset_2 from '../bindings/passed/simple_asset_2'
import * as simple_asset_get_asset1_value from '../bindings/passed/simple_asset_get_asset1_value'
import * as simple_asset_get_asset2_value from '../bindings/passed/simple_asset_get_asset2_value'
import * as simple_asset_get_asset2_value2 from '../bindings/passed/simple_asset_get_asset2_value2'
import * as simple_asset_one_field from '../bindings/passed/simple_asset_one_field'
import * as simple_asset_skip from '../bindings/passed/simple_asset_skip'
import * as simple_asset_skip_empty from '../bindings/passed/simple_asset_skip_empty'
import * as simple_asset_skip_empty_one_field from '../bindings/passed/simple_asset_skip_empty_one_field'
import * as simple_asset_skip_one_field from '../bindings/passed/simple_asset_skip_one_field'
import * as simple_assign1 from '../bindings/passed/simple_assign1'
import * as simple_assign2 from '../bindings/passed/simple_assign2'
import * as simple_assign3 from '../bindings/passed/simple_assign3'
import * as simple_contract_call from '../bindings/passed/simple_contract_call'
import * as simple_freeze from '../bindings/passed/simple_freeze'
import * as simple_fun1 from '../bindings/passed/simple_fun1'
import * as simple_fun2 from '../bindings/passed/simple_fun2'
import * as simple_fun3 from '../bindings/passed/simple_fun3'
import * as simple_fun4 from '../bindings/passed/simple_fun4'
import * as simple_fun5 from '../bindings/passed/simple_fun5'
import * as simple_fun6 from '../bindings/passed/simple_fun6'
import * as simple_fun7 from '../bindings/passed/simple_fun7'
import * as simple_fun8 from '../bindings/passed/simple_fun8'
import * as simple_fun_alt from '../bindings/passed/simple_fun_alt'
import * as simple_fun_with_storage from '../bindings/passed/simple_fun_with_storage'
import * as simple_fun_with_storage2 from '../bindings/passed/simple_fun_with_storage2'
import * as simple_fun_with_storage3 from '../bindings/passed/simple_fun_with_storage3'
import * as simple_get_field from '../bindings/passed/simple_get_field'
import * as simple_if3 from '../bindings/passed/simple_if3'
import * as simple_multi_entry from '../bindings/passed/simple_multi_entry'
import * as simple_multi_entry2 from '../bindings/passed/simple_multi_entry2'
import * as simple_multi_entry3 from '../bindings/passed/simple_multi_entry3'
import * as simple_op_add from '../bindings/passed/simple_op_add'
import * as simple_op_uminus from '../bindings/passed/simple_op_uminus'
import * as simple_record_assign from '../bindings/passed/simple_record_assign'
import * as simple_record_assign1 from '../bindings/passed/simple_record_assign1'
import * as simple_record_assign2 from '../bindings/passed/simple_record_assign2'
import * as simple_record_lit from '../bindings/passed/simple_record_lit'
import * as simple_record_lit_rev from '../bindings/passed/simple_record_lit_rev'
import * as simple_reverse from '../bindings/passed/simple_reverse'
import * as simple_sequence from '../bindings/passed/simple_sequence'
import * as simple_sequence_with_arg from '../bindings/passed/simple_sequence_with_arg'
import * as simple_sequence_with_arg2 from '../bindings/passed/simple_sequence_with_arg2'
import * as simple_sequence_with_arg_var from '../bindings/passed/simple_sequence_with_arg_var'
import * as simple_while from '../bindings/passed/simple_while'
import * as simple_with_arg_view from '../bindings/passed/simple_with_arg_view'
import * as simple_with_type_annot from '../bindings/passed/simple_with_type_annot'
import * as simple_with_view from '../bindings/passed/simple_with_view'
import * as sourced_by from '../bindings/passed/sourced_by'
import * as spec_asset from '../bindings/passed/spec_asset'
import * as spec_definition from '../bindings/passed/spec_definition'
import * as spec_definition_2 from '../bindings/passed/spec_definition_2'
import * as spec_definition_with_param from '../bindings/passed/spec_definition_with_param'
import * as spec_entry from '../bindings/passed/spec_entry'
import * as spec_fail_caller from '../bindings/passed/spec_fail_caller'
import * as spec_fail_source from '../bindings/passed/spec_fail_source'
import * as spec_full from '../bindings/passed/spec_full'
import * as spec_function from '../bindings/passed/spec_function'
import * as spec_predicate from '../bindings/passed/spec_predicate'
import * as spec_variable from '../bindings/passed/spec_variable'
import * as state_in_effect from '../bindings/passed/state_in_effect'
import * as state_is from '../bindings/passed/state_is'
import * as state_var from '../bindings/passed/state_var'
import * as tern_bool_false from '../bindings/passed/tern_bool_false'
import * as tern_bool_true from '../bindings/passed/tern_bool_true'
import * as tern_opt from '../bindings/passed/tern_opt'
import * as tern_opt_3 from '../bindings/passed/tern_opt_3'
import * as test from '../bindings/passed/test'
import * as test_add_asset2_with_partition from '../bindings/passed/test_add_asset2_with_partition'
import * as test_add_asset_with_aggregate from '../bindings/passed/test_add_asset_with_aggregate'
import * as test_add_asset_with_both from '../bindings/passed/test_add_asset_with_both'
import * as test_add_asset_with_partition from '../bindings/passed/test_add_asset_with_partition'
import * as test_addfield_aggregate_1 from '../bindings/passed/test_addfield_aggregate_1'
import * as test_addfield_aggregate_2 from '../bindings/passed/test_addfield_aggregate_2'
import * as test_addfield_partition_1 from '../bindings/passed/test_addfield_partition_1'
import * as test_addfield_partition_2 from '../bindings/passed/test_addfield_partition_2'
import * as test_addupdate_0 from '../bindings/passed/test_addupdate_0'
import * as test_addupdate_1 from '../bindings/passed/test_addupdate_1'
import * as test_addupdate_2 from '../bindings/passed/test_addupdate_2'
import * as test_asset_head_agg_0 from '../bindings/passed/test_asset_head_agg_0'
import * as test_asset_head_agg_1 from '../bindings/passed/test_asset_head_agg_1'
import * as test_asset_head_agg_2 from '../bindings/passed/test_asset_head_agg_2'
import * as test_asset_head_coll_0 from '../bindings/passed/test_asset_head_coll_0'
import * as test_asset_head_coll_1 from '../bindings/passed/test_asset_head_coll_1'
import * as test_asset_head_coll_2 from '../bindings/passed/test_asset_head_coll_2'
import * as test_asset_head_view_0 from '../bindings/passed/test_asset_head_view_0'
import * as test_asset_head_view_1 from '../bindings/passed/test_asset_head_view_1'
import * as test_asset_head_view_2 from '../bindings/passed/test_asset_head_view_2'
import * as test_asset_multi_key from '../bindings/passed/test_asset_multi_key'
import * as test_asset_multi_key_complex from '../bindings/passed/test_asset_multi_key_complex'
import * as test_asset_nth_agg_0 from '../bindings/passed/test_asset_nth_agg_0'
import * as test_asset_nth_agg_1 from '../bindings/passed/test_asset_nth_agg_1'
import * as test_asset_nth_agg_2 from '../bindings/passed/test_asset_nth_agg_2'
import * as test_asset_nth_coll_0 from '../bindings/passed/test_asset_nth_coll_0'
import * as test_asset_nth_coll_1 from '../bindings/passed/test_asset_nth_coll_1'
import * as test_asset_nth_coll_2 from '../bindings/passed/test_asset_nth_coll_2'
import * as test_asset_nth_view_0 from '../bindings/passed/test_asset_nth_view_0'
import * as test_asset_nth_view_1 from '../bindings/passed/test_asset_nth_view_1'
import * as test_asset_nth_view_2 from '../bindings/passed/test_asset_nth_view_2'
import * as test_asset_select_agg_0 from '../bindings/passed/test_asset_select_agg_0'
import * as test_asset_select_agg_1 from '../bindings/passed/test_asset_select_agg_1'
import * as test_asset_select_agg_2 from '../bindings/passed/test_asset_select_agg_2'
import * as test_asset_select_coll_0 from '../bindings/passed/test_asset_select_coll_0'
import * as test_asset_select_coll_1 from '../bindings/passed/test_asset_select_coll_1'
import * as test_asset_select_coll_2 from '../bindings/passed/test_asset_select_coll_2'
import * as test_asset_select_view_0 from '../bindings/passed/test_asset_select_view_0'
import * as test_asset_select_view_1 from '../bindings/passed/test_asset_select_view_1'
import * as test_asset_select_view_2 from '../bindings/passed/test_asset_select_view_2'
import * as test_asset_sort_agg_0 from '../bindings/passed/test_asset_sort_agg_0'
import * as test_asset_sort_agg_1 from '../bindings/passed/test_asset_sort_agg_1'
import * as test_asset_sort_agg_2 from '../bindings/passed/test_asset_sort_agg_2'
import * as test_asset_sort_coll_0 from '../bindings/passed/test_asset_sort_coll_0'
import * as test_asset_sort_coll_1 from '../bindings/passed/test_asset_sort_coll_1'
import * as test_asset_sort_coll_2 from '../bindings/passed/test_asset_sort_coll_2'
import * as test_asset_sort_coll_complex from '../bindings/passed/test_asset_sort_coll_complex'
import * as test_asset_sort_coll_random from '../bindings/passed/test_asset_sort_coll_random'
import * as test_asset_sort_coll_random2 from '../bindings/passed/test_asset_sort_coll_random2'
import * as test_asset_sort_coll_rational from '../bindings/passed/test_asset_sort_coll_rational'
import * as test_asset_sort_view_0 from '../bindings/passed/test_asset_sort_view_0'
import * as test_asset_sort_view_1 from '../bindings/passed/test_asset_sort_view_1'
import * as test_asset_sort_view_2 from '../bindings/passed/test_asset_sort_view_2'
import * as test_asset_sum_agg_0 from '../bindings/passed/test_asset_sum_agg_0'
import * as test_asset_sum_agg_1 from '../bindings/passed/test_asset_sum_agg_1'
import * as test_asset_sum_agg_2 from '../bindings/passed/test_asset_sum_agg_2'
import * as test_asset_sum_coll_0 from '../bindings/passed/test_asset_sum_coll_0'
import * as test_asset_sum_coll_1 from '../bindings/passed/test_asset_sum_coll_1'
import * as test_asset_sum_coll_2 from '../bindings/passed/test_asset_sum_coll_2'
import * as test_asset_sum_coll_rat from '../bindings/passed/test_asset_sum_coll_rat'
import * as test_asset_sum_view_0 from '../bindings/passed/test_asset_sum_view_0'
import * as test_asset_sum_view_1 from '../bindings/passed/test_asset_sum_view_1'
import * as test_asset_sum_view_2 from '../bindings/passed/test_asset_sum_view_2'
import * as test_asset_tail_agg_0 from '../bindings/passed/test_asset_tail_agg_0'
import * as test_asset_tail_agg_1 from '../bindings/passed/test_asset_tail_agg_1'
import * as test_asset_tail_agg_2 from '../bindings/passed/test_asset_tail_agg_2'
import * as test_asset_tail_coll_0 from '../bindings/passed/test_asset_tail_coll_0'
import * as test_asset_tail_coll_1 from '../bindings/passed/test_asset_tail_coll_1'
import * as test_asset_tail_coll_2 from '../bindings/passed/test_asset_tail_coll_2'
import * as test_asset_tail_view_0 from '../bindings/passed/test_asset_tail_view_0'
import * as test_asset_tail_view_1 from '../bindings/passed/test_asset_tail_view_1'
import * as test_asset_tail_view_2 from '../bindings/passed/test_asset_tail_view_2'
import * as test_asset_update from '../bindings/passed/test_asset_update'
import * as test_asset_update_2 from '../bindings/passed/test_asset_update_2'
import * as test_asset_update_3 from '../bindings/passed/test_asset_update_3'
import * as test_asset_update_4 from '../bindings/passed/test_asset_update_4'
import * as test_asset_update_5 from '../bindings/passed/test_asset_update_5'
import * as test_asset_update_aggregate_1 from '../bindings/passed/test_asset_update_aggregate_1'
import * as test_asset_update_aggregate_2 from '../bindings/passed/test_asset_update_aggregate_2'
import * as test_asset_update_aggregate_3 from '../bindings/passed/test_asset_update_aggregate_3'
import * as test_asset_update_partition_1 from '../bindings/passed/test_asset_update_partition_1'
import * as test_asset_update_partition_2 from '../bindings/passed/test_asset_update_partition_2'
import * as test_asset_update_partition_3 from '../bindings/passed/test_asset_update_partition_3'
import * as test_asset_update_partition_4 from '../bindings/passed/test_asset_update_partition_4'
import * as test_asset_update_partition_5 from '../bindings/passed/test_asset_update_partition_5'
import * as test_asset_update_partition_6 from '../bindings/passed/test_asset_update_partition_6'
import * as test_asset_update_partition_7 from '../bindings/passed/test_asset_update_partition_7'
import * as test_asset_update_partition_8 from '../bindings/passed/test_asset_update_partition_8'
import * as test_asset_update_partition_9 from '../bindings/passed/test_asset_update_partition_9'
import * as test_asset_view from '../bindings/passed/test_asset_view'
import * as test_bound_value from '../bindings/passed/test_bound_value'
import * as test_caller_getter from '../bindings/passed/test_caller_getter'
import * as test_cmp_bool from '../bindings/passed/test_cmp_bool'
import * as test_complex_sum from '../bindings/passed/test_complex_sum'
import * as test_conditions from '../bindings/passed/test_conditions'
import * as test_contains_get from '../bindings/passed/test_contains_get'
import * as test_contract from '../bindings/passed/test_contract'
import * as test_contract_self from '../bindings/passed/test_contract_self'
import * as test_fget from '../bindings/passed/test_fget'
import * as test_for_list_alt from '../bindings/passed/test_for_list_alt'
import * as test_fun0 from '../bindings/passed/test_fun0'
import * as test_fun1 from '../bindings/passed/test_fun1'
import * as test_fun2 from '../bindings/passed/test_fun2'
import * as test_fun3 from '../bindings/passed/test_fun3'
import * as test_fun4 from '../bindings/passed/test_fun4'
import * as test_fun5 from '../bindings/passed/test_fun5'
import * as test_fun6 from '../bindings/passed/test_fun6'
import * as test_fun7 from '../bindings/passed/test_fun7'
import * as test_fun8 from '../bindings/passed/test_fun8'
import * as test_fun_asset from '../bindings/passed/test_fun_asset'
import * as test_fun_asset2 from '../bindings/passed/test_fun_asset2'
import * as test_fun_fail from '../bindings/passed/test_fun_fail'
import * as test_getter from '../bindings/passed/test_getter'
import * as test_getter2 from '../bindings/passed/test_getter2'
import * as test_getter_with_arg from '../bindings/passed/test_getter_with_arg'
import * as test_getter_with_args from '../bindings/passed/test_getter_with_args'
import * as test_if_int_nat from '../bindings/passed/test_if_int_nat'
import * as test_init_asset from '../bindings/passed/test_init_asset'
import * as test_init_asset2 from '../bindings/passed/test_init_asset2'
import * as test_init_asset3 from '../bindings/passed/test_init_asset3'
import * as test_init_rat_with_nat from '../bindings/passed/test_init_rat_with_nat'
import * as test_init_storage_arith from '../bindings/passed/test_init_storage_arith'
import * as test_init_storage_cmp from '../bindings/passed/test_init_storage_cmp'
import * as test_init_storage_funs from '../bindings/passed/test_init_storage_funs'
import * as test_init_storage_literals from '../bindings/passed/test_init_storage_literals'
import * as test_init_storage_simple from '../bindings/passed/test_init_storage_simple'
import * as test_initialized_with_asset from '../bindings/passed/test_initialized_with_asset'
import * as test_initializedby from '../bindings/passed/test_initializedby'
import * as test_iter from '../bindings/passed/test_iter'
import * as test_iter2 from '../bindings/passed/test_iter2'
import * as test_length_operations from '../bindings/passed/test_length_operations'
import * as test_list_contains from '../bindings/passed/test_list_contains'
import * as test_list_contains2 from '../bindings/passed/test_list_contains2'
import * as test_list_mynth from '../bindings/passed/test_list_mynth'
import * as test_list_mynth2 from '../bindings/passed/test_list_mynth2'
import * as test_list_mynth3 from '../bindings/passed/test_list_mynth3'
import * as test_list_nth from '../bindings/passed/test_list_nth'
import * as test_metadata from '../bindings/passed/test_metadata'
import * as test_operations from '../bindings/passed/test_operations'
import * as test_oracle from '../bindings/passed/test_oracle'
import * as test_parameter from '../bindings/passed/test_parameter'
import * as test_parameter_const from '../bindings/passed/test_parameter_const'
import * as test_prec from '../bindings/passed/test_prec'
import * as test_quantifiers from '../bindings/passed/test_quantifiers'
import * as test_rational from '../bindings/passed/test_rational'
import * as test_read_asset_after_operation from '../bindings/passed/test_read_asset_after_operation'
import * as test_read_asset_after_update from '../bindings/passed/test_read_asset_after_update'
import * as test_record from '../bindings/passed/test_record'
import * as test_record_access_0 from '../bindings/passed/test_record_access_0'
import * as test_record_access_1 from '../bindings/passed/test_record_access_1'
import * as test_record_access_2 from '../bindings/passed/test_record_access_2'
import * as test_record_access_3 from '../bindings/passed/test_record_access_3'
import * as test_record_assign_1 from '../bindings/passed/test_record_assign_1'
import * as test_record_assign_2 from '../bindings/passed/test_record_assign_2'
import * as test_record_assign_3 from '../bindings/passed/test_record_assign_3'
import * as test_record_assign_full from '../bindings/passed/test_record_assign_full'
import * as test_record_assign_var from '../bindings/passed/test_record_assign_var'
import * as test_record_simple from '../bindings/passed/test_record_simple'
import * as test_remove_asset_with_partition from '../bindings/passed/test_remove_asset_with_partition'
import * as test_removeall_aggregate from '../bindings/passed/test_removeall_aggregate'
import * as test_removeall_aggregate_1 from '../bindings/passed/test_removeall_aggregate_1'
import * as test_removeall_aggregate_2 from '../bindings/passed/test_removeall_aggregate_2'
import * as test_removeall_partition_1 from '../bindings/passed/test_removeall_partition_1'
import * as test_removeall_partition_2 from '../bindings/passed/test_removeall_partition_2'
import * as test_removefield_aggregate_1 from '../bindings/passed/test_removefield_aggregate_1'
import * as test_removefield_aggregate_2 from '../bindings/passed/test_removefield_aggregate_2'
import * as test_removefield_partition_1 from '../bindings/passed/test_removefield_partition_1'
import * as test_removefield_partition_2 from '../bindings/passed/test_removefield_partition_2'
import * as test_removeif_agg_0 from '../bindings/passed/test_removeif_agg_0'
import * as test_removeif_agg_1 from '../bindings/passed/test_removeif_agg_1'
import * as test_removeif_agg_2 from '../bindings/passed/test_removeif_agg_2'
import * as test_removeif_coll_0 from '../bindings/passed/test_removeif_coll_0'
import * as test_removeif_coll_1 from '../bindings/passed/test_removeif_coll_1'
import * as test_removeif_coll_2 from '../bindings/passed/test_removeif_coll_2'
import * as test_removeif_part_0 from '../bindings/passed/test_removeif_part_0'
import * as test_removeif_part_1 from '../bindings/passed/test_removeif_part_1'
import * as test_removeif_part_2 from '../bindings/passed/test_removeif_part_2'
import * as test_result from '../bindings/passed/test_result'
import * as test_security from '../bindings/passed/test_security'
import * as test_specasset from '../bindings/passed/test_specasset'
import * as test_specfun from '../bindings/passed/test_specfun'
import * as test_specvar from '../bindings/passed/test_specvar'
import * as test_tez from '../bindings/passed/test_tez'
import * as test_transfer from '../bindings/passed/test_transfer'
import * as test_transition from '../bindings/passed/test_transition'
import * as test_tuple_access_1 from '../bindings/passed/test_tuple_access_1'
import * as test_tuple_access_2 from '../bindings/passed/test_tuple_access_2'
import * as test_tuple_access_3 from '../bindings/passed/test_tuple_access_3'
import * as test_update from '../bindings/passed/test_update'
import * as test_var from '../bindings/passed/test_var'
import * as test_voting from '../bindings/passed/test_voting'
import * as timelock from '../bindings/passed/timelock'
import * as timelock_var from '../bindings/passed/timelock_var'
import * as transfer_call from '../bindings/passed/transfer_call'
import * as transfer_entrypoint from '../bindings/passed/transfer_entrypoint'
import * as transfer_entrypoint2 from '../bindings/passed/transfer_entrypoint2'
import * as transfer_op from '../bindings/passed/transfer_op'
import * as transfer_require_entrypoint from '../bindings/passed/transfer_require_entrypoint'
import * as transfer_self from '../bindings/passed/transfer_self'
import * as transfer_simple from '../bindings/passed/transfer_simple'
import * as transfer_simple_with_entrypoint from '../bindings/passed/transfer_simple_with_entrypoint'
import * as tuple_in_contains from '../bindings/passed/tuple_in_contains'
import * as type_or from '../bindings/passed/type_or'
import * as type_set_enum_param from '../bindings/passed/type_set_enum_param'
import * as type_storage_or from '../bindings/passed/type_storage_or'
import * as type_tx_rollup_l2_address from '../bindings/passed/type_tx_rollup_l2_address'
import * as typetuple from '../bindings/passed/typetuple'
import * as unused_argument from '../bindings/passed/unused_argument'
import * as unused_variable from '../bindings/passed/unused_variable'
import * as update_minus_equal from '../bindings/passed/update_minus_equal'
import * as var_in_spec from '../bindings/passed/var_in_spec'
import * as var_in_state_inv from '../bindings/passed/var_in_state_inv'
import * as var_without_effect from '../bindings/passed/var_without_effect'
import * as variable_in_container from '../bindings/passed/variable_in_container'
import * as verif_simple from '../bindings/passed/verif_simple'
import * as very_simple from '../bindings/passed/very_simple'
import * as view_0 from '../bindings/passed/view_0'
import * as view_all_chain from '../bindings/passed/view_all_chain'
import * as view_args_0 from '../bindings/passed/view_args_0'
import * as view_args_1 from '../bindings/passed/view_args_1'
import * as view_args_storage_0 from '../bindings/passed/view_args_storage_0'
import * as view_args_storage_1 from '../bindings/passed/view_args_storage_1'
import * as view_asset from '../bindings/passed/view_asset'
import * as view_exhaustive from '../bindings/passed/view_exhaustive'
import * as view_in_arg from '../bindings/passed/view_in_arg'
import * as view_offchain from '../bindings/passed/view_offchain'
import * as view_offchain_nat from '../bindings/passed/view_offchain_nat'
import * as view_onchain from '../bindings/passed/view_onchain'
import * as view_onchain_offchain from '../bindings/passed/view_onchain_offchain'
import * as view_simple from '../bindings/passed/view_simple'
import * as view_simple_caller from '../bindings/passed/view_simple_caller'
import * as view_storage_0 from '../bindings/passed/view_storage_0'
import * as view_storage_1 from '../bindings/passed/view_storage_1'
import * as view_storage_2 from '../bindings/passed/view_storage_2'
import * as view_storage_3 from '../bindings/passed/view_storage_3'
import * as view_storage_4 from '../bindings/passed/view_storage_4'
import * as view_storage_5 from '../bindings/passed/view_storage_5'
import * as with_metadata_json from '../bindings/passed/with_metadata_json'
import * as with_metadata_json_with_offchain_view from '../bindings/passed/with_metadata_json_with_offchain_view'
import * as with_metadata_uri from '../bindings/passed/with_metadata_uri'


const alice = get_account('alice')

/* Verbose mode ------------------------------------------------------------ */

set_quiet(true);

/* Endpoint ---------------------------------------------------------------- */

set_mockup()

/* Tests-------------------------------------------------------------------- */

describe('Tests', async () => {
  describe('Passed', async () => {

    it('add_update_record', async () => {
      await add_update_record.add_update_record.deploy({ as: alice })
      const res_before = await add_update_record.add_update_record.get_my_asset();
      assert(res_before.length == 1, "Invalid Value")
      assert(res_before[0][0].equals(new Nat(0)), "Invalid Value")
      assert(res_before[0][1].b == false, "Invalid Value")
      assert(res_before[0][1].c.length == 0, "Invalid Value")
      await add_update_record.add_update_record.updateTransferlist({ as: alice })
      const res_after = await add_update_record.add_update_record.get_my_asset()
      assert(res_after.length == 1, "Invalid Value")
      assert(res_after[0][0].equals(new Nat(0)), "Invalid Value")
      assert(res_after[0][1].b == true, "Invalid Value")
      assert(res_after[0][1].c.length == 3, "Invalid Value")
      assert(res_after[0][1].c[0].equals(new Nat(0)), "Invalid Value")
      assert(res_after[0][1].c[1].equals(new Nat(1)), "Invalid Value")
      assert(res_after[0][1].c[2].equals(new Nat(2)), "Invalid Value")
    })

    // TODO
    it('addupdate_partition', async () => {
      await addupdate_partition.addupdate_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await addupdate_partition.addupdate_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await addupdate_partition.addupdate_partition.exec({ as: alice })
      //      const res_after = await addupdate_partition.addupdate_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('addupdate_partition2', async () => {
      await addupdate_partition2.addupdate_partition2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await addupdate_partition2.addupdate_partition2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await addupdate_partition2.addupdate_partition2.exec({ as: alice })
      //      const res_after = await addupdate_partition2.addupdate_partition2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('addupdate_partition_with_no_effect_on_default_value', async () => {
      await addupdate_partition_with_no_effect_on_default_value.addupdate_partition_with_no_effect_on_default_value.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await addupdate_partition_with_no_effect_on_default_value.addupdate_partition_with_no_effect_on_default_value.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await addupdate_partition_with_no_effect_on_default_value.addupdate_partition_with_no_effect_on_default_value.exec({ as: alice })
      //      const res_after = await addupdate_partition_with_no_effect_on_default_value.addupdate_partition_with_no_effect_on_default_value.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('addupdate_with_no_effect_on_default_value', async () => {
      await addupdate_with_no_effect_on_default_value.addupdate_with_no_effect_on_default_value.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await addupdate_with_no_effect_on_default_value.addupdate_with_no_effect_on_default_value.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await addupdate_with_no_effect_on_default_value.addupdate_with_no_effect_on_default_value.exec({ as: alice })
      //      const res_after = await addupdate_with_no_effect_on_default_value.addupdate_with_no_effect_on_default_value.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('annot_enum', async () => {
      await annot_enum.annot_enum.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await annot_enum.annot_enum.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await annot_enum.annot_enum.exec({ as: alice })
      //      const res_after = await annot_enum.annot_enum.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('apply_lambda', async () => {
      await apply_lambda.apply_lambda.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await apply_lambda.apply_lambda.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await apply_lambda.apply_lambda.exec({ as: alice })
      //      const res_after = await apply_lambda.apply_lambda.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('arith_bls', async () => {
      await arith_bls.arith_bls.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await arith_bls.arith_bls.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await arith_bls.arith_bls.exec({ as: alice })
      //      const res_after = await arith_bls.arith_bls.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('arith_tez', async () => {
      await arith_tez.arith_tez.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await arith_tez.arith_tez.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await arith_tez.arith_tez.exec({ as: alice })
      //      const res_after = await arith_tez.arith_tez.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('ascii_string', async () => {
      await ascii_string.ascii_string.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await ascii_string.ascii_string.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await ascii_string.ascii_string.exec({ as: alice })
      //      const res_after = await ascii_string.ascii_string.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_access', async () => {
      await asset_access.asset_access.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_access.asset_access.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_access.asset_access.exec({ as: alice })
      //      const res_after = await asset_access.asset_access.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_access_basic', async () => {
      await asset_access_basic.asset_access_basic.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_access_basic.asset_access_basic.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_access_basic.asset_access_basic.exec({ as: alice })
      //      const res_after = await asset_access_basic.asset_access_basic.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_access_option_found', async () => {
      await asset_access_option_found.asset_access_option_found.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_access_option_found.asset_access_option_found.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_access_option_found.asset_access_option_found.exec({ as: alice })
      //      const res_after = await asset_access_option_found.asset_access_option_found.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_access_option_not_found', async () => {
      await asset_access_option_not_found.asset_access_option_not_found.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_access_option_not_found.asset_access_option_not_found.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_access_option_not_found.asset_access_option_not_found.exec({ as: alice })
      //      const res_after = await asset_access_option_not_found.asset_access_option_not_found.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_access_value', async () => {
      await asset_access_value.asset_access_value.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_access_value.asset_access_value.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_access_value.asset_access_value.exec({ as: alice })
      //      const res_after = await asset_access_value.asset_access_value.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_addupdate', async () => {
      await asset_addupdate.asset_addupdate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_addupdate.asset_addupdate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_addupdate.asset_addupdate.exec({ as: alice })
      //      const res_after = await asset_addupdate.asset_addupdate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_big_map', async () => {
      await asset_big_map.asset_big_map.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_big_map.asset_big_map.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_big_map.asset_big_map.exec({ as: alice })
      //      const res_after = await asset_big_map.asset_big_map.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_big_map_unit_effect_add', async () => {
      await asset_big_map_unit_effect_add.asset_big_map_unit_effect_add.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_big_map_unit_effect_add.asset_big_map_unit_effect_add.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_big_map_unit_effect_add.asset_big_map_unit_effect_add.exec({ as: alice })
      //      const res_after = await asset_big_map_unit_effect_add.asset_big_map_unit_effect_add.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_big_map_unit_effect_addupdate', async () => {
      await asset_big_map_unit_effect_addupdate.asset_big_map_unit_effect_addupdate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_big_map_unit_effect_addupdate.asset_big_map_unit_effect_addupdate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_big_map_unit_effect_addupdate.asset_big_map_unit_effect_addupdate.exec({ as: alice })
      //      const res_after = await asset_big_map_unit_effect_addupdate.asset_big_map_unit_effect_addupdate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_big_map_unit_effect_remove', async () => {
      await asset_big_map_unit_effect_remove.asset_big_map_unit_effect_remove.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_big_map_unit_effect_remove.asset_big_map_unit_effect_remove.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_big_map_unit_effect_remove.asset_big_map_unit_effect_remove.exec({ as: alice })
      //      const res_after = await asset_big_map_unit_effect_remove.asset_big_map_unit_effect_remove.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_big_map_unit_effect_removeall', async () => {
      await asset_big_map_unit_effect_removeall.asset_big_map_unit_effect_removeall.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_big_map_unit_effect_removeall.asset_big_map_unit_effect_removeall.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_big_map_unit_effect_removeall.asset_big_map_unit_effect_removeall.exec({ as: alice })
      //      const res_after = await asset_big_map_unit_effect_removeall.asset_big_map_unit_effect_removeall.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_big_map_unit_effect_update', async () => {
      await asset_big_map_unit_effect_update.asset_big_map_unit_effect_update.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_big_map_unit_effect_update.asset_big_map_unit_effect_update.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_big_map_unit_effect_update.asset_big_map_unit_effect_update.exec({ as: alice })
      //      const res_after = await asset_big_map_unit_effect_update.asset_big_map_unit_effect_update.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_big_map_unit_expression_contains', async () => {
      await asset_big_map_unit_expression_contains.asset_big_map_unit_expression_contains.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_big_map_unit_expression_contains.asset_big_map_unit_expression_contains.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_big_map_unit_expression_contains.asset_big_map_unit_expression_contains.exec({ as: alice })
      //      const res_after = await asset_big_map_unit_expression_contains.asset_big_map_unit_expression_contains.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_big_map_unit_storage', async () => {
      await asset_big_map_unit_storage.asset_big_map_unit_storage.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_big_map_unit_storage.asset_big_map_unit_storage.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_big_map_unit_storage.asset_big_map_unit_storage.exec({ as: alice })
      //      const res_after = await asset_big_map_unit_storage.asset_big_map_unit_storage.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_for', async () => {
      await asset_for.asset_for.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_for.asset_for.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_for.asset_for.exec({ as: alice })
      //      const res_after = await asset_for.asset_for.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_init_by_const_key', async () => {
      await asset_init_by_const_key.asset_init_by_const_key.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_init_by_const_key.asset_init_by_const_key.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_init_by_const_key.asset_init_by_const_key.exec({ as: alice })
      //      const res_after = await asset_init_by_const_key.asset_init_by_const_key.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_init_by_const_key_parameter', async () => {
      await asset_init_by_const_key_parameter.asset_init_by_const_key_parameter.deploy(new Nat(1), { as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_init_by_const_key_parameter.asset_init_by_const_key_parameter.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_init_by_const_key_parameter.asset_init_by_const_key_parameter.exec({ as: alice })
      //      const res_after = await asset_init_by_const_key_parameter.asset_init_by_const_key_parameter.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_initializedby_aggregate_empty', async () => {
      await asset_initializedby_aggregate_empty.asset_initializedby_aggregate_empty.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_initializedby_aggregate_empty.asset_initializedby_aggregate_empty.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_initializedby_aggregate_empty.asset_initializedby_aggregate_empty.exec({ as: alice })
      //      const res_after = await asset_initializedby_aggregate_empty.asset_initializedby_aggregate_empty.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_initializedby_aggregate_filled', async () => {
      await asset_initializedby_aggregate_filled.asset_initializedby_aggregate_filled.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_initializedby_aggregate_filled.asset_initializedby_aggregate_filled.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_initializedby_aggregate_filled.asset_initializedby_aggregate_filled.exec({ as: alice })
      //      const res_after = await asset_initializedby_aggregate_filled.asset_initializedby_aggregate_filled.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_key_in_record', async () => {
      await asset_key_in_record.asset_key_in_record.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_key_in_record.asset_key_in_record.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_key_in_record.asset_key_in_record.exec({ as: alice })
      //      const res_after = await asset_key_in_record.asset_key_in_record.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_key_tuple', async () => {
      await asset_key_tuple.asset_key_tuple.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_key_tuple.asset_key_tuple.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_key_tuple.asset_key_tuple.exec({ as: alice })
      //      const res_after = await asset_key_tuple.asset_key_tuple.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_not_found', async () => {
      await asset_not_found.asset_not_found.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_not_found.asset_not_found.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_not_found.asset_not_found.exec({ as: alice })
      //      const res_after = await asset_not_found.asset_not_found.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_nth', async () => {
      await asset_nth.asset_nth.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_nth.asset_nth.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_nth.asset_nth.exec({ as: alice })
      //      const res_after = await asset_nth.asset_nth.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_put_single', async () => {
      await asset_put_single.asset_put_single.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_put_single.asset_put_single.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_put_single.asset_put_single.exec({ as: alice })
      //      const res_after = await asset_put_single.asset_put_single.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_simple', async () => {
      await asset_simple.asset_simple.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_simple.asset_simple.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_simple.asset_simple.exec({ as: alice })
      //      const res_after = await asset_simple.asset_simple.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_simple_to_big_map', async () => {
      await asset_simple_to_big_map.asset_simple_to_big_map.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_simple_to_big_map.asset_simple_to_big_map.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_simple_to_big_map.asset_simple_to_big_map.exec({ as: alice })
      //      const res_after = await asset_simple_to_big_map.asset_simple_to_big_map.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_tern_opt', async () => {
      await asset_tern_opt.asset_tern_opt.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_tern_opt.asset_tern_opt.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_tern_opt.asset_tern_opt.exec({ as: alice })
      //      const res_after = await asset_tern_opt.asset_tern_opt.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_ternary_expr_found', async () => {
      await asset_ternary_expr_found.asset_ternary_expr_found.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_ternary_expr_found.asset_ternary_expr_found.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_ternary_expr_found.asset_ternary_expr_found.exec({ as: alice })
      //      const res_after = await asset_ternary_expr_found.asset_ternary_expr_found.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_ternary_expr_notfound', async () => {
      await asset_ternary_expr_notfound.asset_ternary_expr_notfound.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_ternary_expr_notfound.asset_ternary_expr_notfound.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_ternary_expr_notfound.asset_ternary_expr_notfound.exec({ as: alice })
      //      const res_after = await asset_ternary_expr_notfound.asset_ternary_expr_notfound.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_types_get', async () => {
      await asset_types_get.asset_types_get.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_types_get.asset_types_get.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_types_get.asset_types_get.exec({ as: alice })
      //      const res_after = await asset_types_get.asset_types_get.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_update_with_basic_container_map_lit_add', async () => {
      await asset_update_with_basic_container_map_lit_add.asset_update_with_basic_container_map_lit_add.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_update_with_basic_container_map_lit_add.asset_update_with_basic_container_map_lit_add.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_update_with_basic_container_map_lit_add.asset_update_with_basic_container_map_lit_add.exec({ as: alice })
      //      const res_after = await asset_update_with_basic_container_map_lit_add.asset_update_with_basic_container_map_lit_add.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_update_with_basic_container_map_lit_remove', async () => {
      await asset_update_with_basic_container_map_lit_remove.asset_update_with_basic_container_map_lit_remove.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_update_with_basic_container_map_lit_remove.asset_update_with_basic_container_map_lit_remove.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_update_with_basic_container_map_lit_remove.asset_update_with_basic_container_map_lit_remove.exec({ as: alice })
      //      const res_after = await asset_update_with_basic_container_map_lit_remove.asset_update_with_basic_container_map_lit_remove.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_update_with_basic_container_map_var_list_add', async () => {
      await asset_update_with_basic_container_map_var_list_add.asset_update_with_basic_container_map_var_list_add.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_update_with_basic_container_map_var_list_add.asset_update_with_basic_container_map_var_list_add.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_update_with_basic_container_map_var_list_add.asset_update_with_basic_container_map_var_list_add.exec({ as: alice })
      //      const res_after = await asset_update_with_basic_container_map_var_list_add.asset_update_with_basic_container_map_var_list_add.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_update_with_basic_container_map_var_list_remove', async () => {
      await asset_update_with_basic_container_map_var_list_remove.asset_update_with_basic_container_map_var_list_remove.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_update_with_basic_container_map_var_list_remove.asset_update_with_basic_container_map_var_list_remove.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_update_with_basic_container_map_var_list_remove.asset_update_with_basic_container_map_var_list_remove.exec({ as: alice })
      //      const res_after = await asset_update_with_basic_container_map_var_list_remove.asset_update_with_basic_container_map_var_list_remove.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_update_with_basic_container_map_var_set_add', async () => {
      await asset_update_with_basic_container_map_var_set_add.asset_update_with_basic_container_map_var_set_add.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_update_with_basic_container_map_var_set_add.asset_update_with_basic_container_map_var_set_add.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_update_with_basic_container_map_var_set_add.asset_update_with_basic_container_map_var_set_add.exec({ as: alice })
      //      const res_after = await asset_update_with_basic_container_map_var_set_add.asset_update_with_basic_container_map_var_set_add.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_update_with_basic_container_map_var_set_remove', async () => {
      await asset_update_with_basic_container_map_var_set_remove.asset_update_with_basic_container_map_var_set_remove.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_update_with_basic_container_map_var_set_remove.asset_update_with_basic_container_map_var_set_remove.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_update_with_basic_container_map_var_set_remove.asset_update_with_basic_container_map_var_set_remove.exec({ as: alice })
      //      const res_after = await asset_update_with_basic_container_map_var_set_remove.asset_update_with_basic_container_map_var_set_remove.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_update_with_basic_container_set_lit_add', async () => {
      await asset_update_with_basic_container_set_lit_add.asset_update_with_basic_container_set_lit_add.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_update_with_basic_container_set_lit_add.asset_update_with_basic_container_set_lit_add.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_update_with_basic_container_set_lit_add.asset_update_with_basic_container_set_lit_add.exec({ as: alice })
      //      const res_after = await asset_update_with_basic_container_set_lit_add.asset_update_with_basic_container_set_lit_add.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_update_with_basic_container_set_lit_remove', async () => {
      await asset_update_with_basic_container_set_lit_remove.asset_update_with_basic_container_set_lit_remove.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_update_with_basic_container_set_lit_remove.asset_update_with_basic_container_set_lit_remove.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_update_with_basic_container_set_lit_remove.asset_update_with_basic_container_set_lit_remove.exec({ as: alice })
      //      const res_after = await asset_update_with_basic_container_set_lit_remove.asset_update_with_basic_container_set_lit_remove.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_update_with_basic_container_set_var_list_add', async () => {
      await asset_update_with_basic_container_set_var_list_add.asset_update_with_basic_container_set_var_list_add.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_update_with_basic_container_set_var_list_add.asset_update_with_basic_container_set_var_list_add.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_update_with_basic_container_set_var_list_add.asset_update_with_basic_container_set_var_list_add.exec({ as: alice })
      //      const res_after = await asset_update_with_basic_container_set_var_list_add.asset_update_with_basic_container_set_var_list_add.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_update_with_basic_container_set_var_list_remove', async () => {
      await asset_update_with_basic_container_set_var_list_remove.asset_update_with_basic_container_set_var_list_remove.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_update_with_basic_container_set_var_list_remove.asset_update_with_basic_container_set_var_list_remove.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_update_with_basic_container_set_var_list_remove.asset_update_with_basic_container_set_var_list_remove.exec({ as: alice })
      //      const res_after = await asset_update_with_basic_container_set_var_list_remove.asset_update_with_basic_container_set_var_list_remove.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_update_with_basic_container_set_var_set_add', async () => {
      await asset_update_with_basic_container_set_var_set_add.asset_update_with_basic_container_set_var_set_add.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_update_with_basic_container_set_var_set_add.asset_update_with_basic_container_set_var_set_add.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_update_with_basic_container_set_var_set_add.asset_update_with_basic_container_set_var_set_add.exec({ as: alice })
      //      const res_after = await asset_update_with_basic_container_set_var_set_add.asset_update_with_basic_container_set_var_set_add.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('asset_update_with_basic_container_set_var_set_remove', async () => {
      await asset_update_with_basic_container_set_var_set_remove.asset_update_with_basic_container_set_var_set_remove.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await asset_update_with_basic_container_set_var_set_remove.asset_update_with_basic_container_set_var_set_remove.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await asset_update_with_basic_container_set_var_set_remove.asset_update_with_basic_container_set_var_set_remove.exec({ as: alice })
      //      const res_after = await asset_update_with_basic_container_set_var_set_remove.asset_update_with_basic_container_set_var_set_remove.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('assign_add_record', async () => {
      await assign_add_record.assign_add_record.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await assign_add_record.assign_add_record.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await assign_add_record.assign_add_record.exec({ as: alice })
      //      const res_after = await assign_add_record.assign_add_record.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('assign_add_tuple', async () => {
      await assign_add_tuple.assign_add_tuple.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await assign_add_tuple.assign_add_tuple.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await assign_add_tuple.assign_add_tuple.exec({ as: alice })
      //      const res_after = await assign_add_tuple.assign_add_tuple.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('assign_field', async () => {
      await assign_field.assign_field.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await assign_field.assign_field.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await assign_field.assign_field.exec({ as: alice })
      //      const res_after = await assign_field.assign_field.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('assign_minus_nat', async () => {
      await assign_minus_nat.assign_minus_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await assign_minus_nat.assign_minus_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await assign_minus_nat.assign_minus_nat.exec({ as: alice })
      //      const res_after = await assign_minus_nat.assign_minus_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('assign_opt', async () => {
      await assign_opt.assign_opt.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await assign_opt.assign_opt.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await assign_opt.assign_opt.exec({ as: alice })
      //      const res_after = await assign_opt.assign_opt.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('assign_var_rat_int', async () => {
      await assign_var_rat_int.assign_var_rat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await assign_var_rat_int.assign_var_rat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await assign_var_rat_int.assign_var_rat_int.exec({ as: alice })
      //      const res_after = await assign_var_rat_int.assign_var_rat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('assign_vardecl_rat_int', async () => {
      await assign_vardecl_rat_int.assign_vardecl_rat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await assign_vardecl_rat_int.assign_vardecl_rat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await assign_vardecl_rat_int.assign_vardecl_rat_int.exec({ as: alice })
      //      const res_after = await assign_vardecl_rat_int.assign_vardecl_rat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('assign_vardecl_rat_nat', async () => {
      await assign_vardecl_rat_nat.assign_vardecl_rat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await assign_vardecl_rat_nat.assign_vardecl_rat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await assign_vardecl_rat_nat.assign_vardecl_rat_nat.exec({ as: alice })
      //      const res_after = await assign_vardecl_rat_nat.assign_vardecl_rat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('before_asset_api', async () => {
      await before_asset_api.before_asset_api.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await before_asset_api.before_asset_api.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await before_asset_api.before_asset_api.exec({ as: alice })
      //      const res_after = await before_asset_api.before_asset_api.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('before_var', async () => {
      await before_var.before_var.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await before_var.before_var.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await before_var.before_var.exec({ as: alice })
      //      const res_after = await before_var.before_var.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('builtin_in_function', async () => {
      await builtin_in_function.builtin_in_function.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await builtin_in_function.builtin_in_function.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await builtin_in_function.builtin_in_function.exec({ as: alice })
      //      const res_after = await builtin_in_function.builtin_in_function.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('called_by_an_asset', async () => {
      await called_by_an_asset.called_by_an_asset.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await called_by_an_asset.called_by_an_asset.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await called_by_an_asset.called_by_an_asset.exec({ as: alice })
      //      const res_after = await called_by_an_asset.called_by_an_asset.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('cast', async () => {
      await cast.cast.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await cast.cast.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await cast.cast.exec({ as: alice })
      //      const res_after = await cast.cast.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('cast_dur_int', async () => {
      await cast_dur_int.cast_dur_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await cast_dur_int.cast_dur_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await cast_dur_int.cast_dur_int.exec({ as: alice })
      //      const res_after = await cast_dur_int.cast_dur_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('cast_nat_int', async () => {
      await cast_nat_int.cast_nat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await cast_nat_int.cast_nat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await cast_nat_int.cast_nat_int.exec({ as: alice })
      //      const res_after = await cast_nat_int.cast_nat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('cast_nat_int_lit', async () => {
      await cast_nat_int_lit.cast_nat_int_lit.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await cast_nat_int_lit.cast_nat_int_lit.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await cast_nat_int_lit.cast_nat_int_lit.exec({ as: alice })
      //      const res_after = await cast_nat_int_lit.cast_nat_int_lit.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('cast_return', async () => {
      await cast_return.cast_return.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await cast_return.cast_return.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await cast_return.cast_return.exec({ as: alice })
      //      const res_after = await cast_return.cast_return.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('cast_view_pklist', async () => {
      await cast_view_pklist.cast_view_pklist.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await cast_view_pklist.cast_view_pklist.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await cast_view_pklist.cast_view_pklist.exec({ as: alice })
      //      const res_after = await cast_view_pklist.cast_view_pklist.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('col_iter_direct_storage', async () => {
      await col_iter_direct_storage.col_iter_direct_storage.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await col_iter_direct_storage.col_iter_direct_storage.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await col_iter_direct_storage.col_iter_direct_storage.exec({ as: alice })
      //      const res_after = await col_iter_direct_storage.col_iter_direct_storage.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('col_iter_filter_storage', async () => {
      await col_iter_filter_storage.col_iter_filter_storage.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await col_iter_filter_storage.col_iter_filter_storage.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await col_iter_filter_storage.col_iter_filter_storage.exec({ as: alice })
      //      const res_after = await col_iter_filter_storage.col_iter_filter_storage.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('compare_enum', async () => {
      await compare_enum.compare_enum.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await compare_enum.compare_enum.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await compare_enum.compare_enum.exec({ as: alice })
      //      const res_after = await compare_enum.compare_enum.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('const_decl', async () => {
      await const_decl.const_decl.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await const_decl.const_decl.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await const_decl.const_decl.exec({ as: alice })
      //      const res_after = await const_decl.const_decl.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('contract_called', async () => {
      await contract_called.contract_called.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await contract_called.contract_called.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await contract_called.contract_called.exec({ as: alice })
      //      const res_after = await contract_called.contract_called.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('contract_caller', async () => {
      await contract_caller.contract_caller.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await contract_caller.contract_caller.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await contract_caller.contract_caller.exec({ as: alice })
      //      const res_after = await contract_caller.contract_caller.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('contract_empty', async () => {
      await contract_empty.contract_empty.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await contract_empty.contract_empty.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await contract_empty.contract_empty.exec({ as: alice })
      //      const res_after = await contract_empty.contract_empty.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('contract_to_address', async () => {
      await contract_to_address.contract_to_address.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await contract_to_address.contract_to_address.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await contract_to_address.contract_to_address.exec({ as: alice })
      //      const res_after = await contract_to_address.contract_to_address.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('contract_transition', async () => {
      await contract_transition.contract_transition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await contract_transition.contract_transition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await contract_transition.contract_transition.exec({ as: alice })
      //      const res_after = await contract_transition.contract_transition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('contract_transition_on_asset', async () => {
      await contract_transition_on_asset.contract_transition_on_asset.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await contract_transition_on_asset.contract_transition_on_asset.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await contract_transition_on_asset.contract_transition_on_asset.exec({ as: alice })
      //      const res_after = await contract_transition_on_asset.contract_transition_on_asset.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('counter', async () => {
      await counter.counter.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await counter.counter.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await counter.counter.exec({ as: alice })
      //      const res_after = await counter.counter.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('counter_proxy', async () => {
      await counter_proxy.counter_proxy.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await counter_proxy.counter_proxy.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await counter_proxy.counter_proxy.exec({ as: alice })
      //      const res_after = await counter_proxy.counter_proxy.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('custom_args_with_record', async () => {
      await custom_args_with_record.custom_args_with_record.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await custom_args_with_record.custom_args_with_record.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await custom_args_with_record.custom_args_with_record.exec({ as: alice })
      //      const res_after = await custom_args_with_record.custom_args_with_record.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('custom_storage', async () => {
      await custom_storage.custom_storage.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await custom_storage.custom_storage.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await custom_storage.custom_storage.exec({ as: alice })
      //      const res_after = await custom_storage.custom_storage.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('custom_storage10', async () => {
      await custom_storage10.custom_storage10.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await custom_storage10.custom_storage10.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await custom_storage10.custom_storage10.exec({ as: alice })
      //      const res_after = await custom_storage10.custom_storage10.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('custom_storage2', async () => {
      await custom_storage2.custom_storage2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await custom_storage2.custom_storage2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await custom_storage2.custom_storage2.exec({ as: alice })
      //      const res_after = await custom_storage2.custom_storage2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('custom_storage3', async () => {
      await custom_storage3.custom_storage3.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await custom_storage3.custom_storage3.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await custom_storage3.custom_storage3.exec({ as: alice })
      //      const res_after = await custom_storage3.custom_storage3.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('custom_storage4', async () => {
      await custom_storage4.custom_storage4.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await custom_storage4.custom_storage4.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await custom_storage4.custom_storage4.exec({ as: alice })
      //      const res_after = await custom_storage4.custom_storage4.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('custom_storage5', async () => {
      await custom_storage5.custom_storage5.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await custom_storage5.custom_storage5.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await custom_storage5.custom_storage5.exec({ as: alice })
      //      const res_after = await custom_storage5.custom_storage5.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('custom_storage6', async () => {
      await custom_storage6.custom_storage6.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await custom_storage6.custom_storage6.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await custom_storage6.custom_storage6.exec({ as: alice })
      //      const res_after = await custom_storage6.custom_storage6.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('custom_storage7', async () => {
      await custom_storage7.custom_storage7.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await custom_storage7.custom_storage7.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await custom_storage7.custom_storage7.exec({ as: alice })
      //      const res_after = await custom_storage7.custom_storage7.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('custom_storage8', async () => {
      await custom_storage8.custom_storage8.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await custom_storage8.custom_storage8.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await custom_storage8.custom_storage8.exec({ as: alice })
      //      const res_after = await custom_storage8.custom_storage8.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('custom_storage9', async () => {
      await custom_storage9.custom_storage9.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await custom_storage9.custom_storage9.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await custom_storage9.custom_storage9.exec({ as: alice })
      //      const res_after = await custom_storage9.custom_storage9.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('dangling_else', async () => {
      await dangling_else.dangling_else.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await dangling_else.dangling_else.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await dangling_else.dangling_else.exec({ as: alice })
      //      const res_after = await dangling_else.dangling_else.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('dec_lit', async () => {
      await dec_lit.dec_lit.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await dec_lit.dec_lit.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await dec_lit.dec_lit.exec({ as: alice })
      //      const res_after = await dec_lit.dec_lit.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('decl_var_opt', async () => {
      await decl_var_opt.decl_var_opt.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await decl_var_opt.decl_var_opt.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await decl_var_opt.decl_var_opt.exec({ as: alice })
      //      const res_after = await decl_var_opt.decl_var_opt.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('decl_var_opt_default', async () => {
      await decl_var_opt_default.decl_var_opt_default.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await decl_var_opt_default.decl_var_opt_default.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await decl_var_opt_default.decl_var_opt_default.exec({ as: alice })
      //      const res_after = await decl_var_opt_default.decl_var_opt_default.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('decomp_if', async () => {
      await decomp_if.decomp_if.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await decomp_if.decomp_if.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await decomp_if.decomp_if.exec({ as: alice })
      //      const res_after = await decomp_if.decomp_if.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('decomp_if2', async () => {
      await decomp_if2.decomp_if2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await decomp_if2.decomp_if2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await decomp_if2.decomp_if2.exec({ as: alice })
      //      const res_after = await decomp_if2.decomp_if2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('decomp_if3', async () => {
      await decomp_if3.decomp_if3.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await decomp_if3.decomp_if3.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await decomp_if3.decomp_if3.exec({ as: alice })
      //      const res_after = await decomp_if3.decomp_if3.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('decomp_if4', async () => {
      await decomp_if4.decomp_if4.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await decomp_if4.decomp_if4.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await decomp_if4.decomp_if4.exec({ as: alice })
      //      const res_after = await decomp_if4.decomp_if4.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('decomp_ifexpr', async () => {
      await decomp_ifexpr.decomp_ifexpr.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await decomp_ifexpr.decomp_ifexpr.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await decomp_ifexpr.decomp_ifexpr.exec({ as: alice })
      //      const res_after = await decomp_ifexpr.decomp_ifexpr.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('decomp_map', async () => {
      await decomp_map.decomp_map.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await decomp_map.decomp_map.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await decomp_map.decomp_map.exec({ as: alice })
      //      const res_after = await decomp_map.decomp_map.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('decomp_test', async () => {
      await decomp_test.decomp_test.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await decomp_test.decomp_test.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await decomp_test.decomp_test.exec({ as: alice })
      //      const res_after = await decomp_test.decomp_test.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('decomp_test2', async () => {
      await decomp_test2.decomp_test2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await decomp_test2.decomp_test2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await decomp_test2.decomp_test2.exec({ as: alice })
      //      const res_after = await decomp_test2.decomp_test2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('decomp_while', async () => {
      await decomp_while.decomp_while.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await decomp_while.decomp_while.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await decomp_while.decomp_while.exec({ as: alice })
      //      const res_after = await decomp_while.decomp_while.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('decomp_while1', async () => {
      await decomp_while1.decomp_while1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await decomp_while1.decomp_while1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await decomp_while1.decomp_while1.exec({ as: alice })
      //      const res_after = await decomp_while1.decomp_while1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('decomp_while2', async () => {
      await decomp_while2.decomp_while2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await decomp_while2.decomp_while2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await decomp_while2.decomp_while2.exec({ as: alice })
      //      const res_after = await decomp_while2.decomp_while2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('duration_to_int', async () => {
      await duration_to_int.duration_to_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await duration_to_int.duration_to_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await duration_to_int.duration_to_int.exec({ as: alice })
      //      const res_after = await duration_to_int.duration_to_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_add_asset_with_complex_partition', async () => {
      await effect_add_asset_with_complex_partition.effect_add_asset_with_complex_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_add_asset_with_complex_partition.effect_add_asset_with_complex_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_add_asset_with_complex_partition.effect_add_asset_with_complex_partition.exec({ as: alice })
      //      const res_after = await effect_add_asset_with_complex_partition.effect_add_asset_with_complex_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_control_for_aggregate', async () => {
      await effect_control_for_aggregate.effect_control_for_aggregate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_control_for_aggregate.effect_control_for_aggregate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_control_for_aggregate.effect_control_for_aggregate.exec({ as: alice })
      //      const res_after = await effect_control_for_aggregate.effect_control_for_aggregate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_control_for_collection', async () => {
      await effect_control_for_collection.effect_control_for_collection.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_control_for_collection.effect_control_for_collection.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_control_for_collection.effect_control_for_collection.exec({ as: alice })
      //      const res_after = await effect_control_for_collection.effect_control_for_collection.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_control_for_collection_one_field', async () => {
      await effect_control_for_collection_one_field.effect_control_for_collection_one_field.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_control_for_collection_one_field.effect_control_for_collection_one_field.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_control_for_collection_one_field.effect_control_for_collection_one_field.exec({ as: alice })
      //      const res_after = await effect_control_for_collection_one_field.effect_control_for_collection_one_field.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_control_for_list', async () => {
      await effect_control_for_list.effect_control_for_list.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_control_for_list.effect_control_for_list.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_control_for_list.effect_control_for_list.exec({ as: alice })
      //      const res_after = await effect_control_for_list.effect_control_for_list.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_control_for_map', async () => {
      await effect_control_for_map.effect_control_for_map.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_control_for_map.effect_control_for_map.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_control_for_map.effect_control_for_map.exec({ as: alice })
      //      const res_after = await effect_control_for_map.effect_control_for_map.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_control_for_partition', async () => {
      await effect_control_for_partition.effect_control_for_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_control_for_partition.effect_control_for_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_control_for_partition.effect_control_for_partition.exec({ as: alice })
      //      const res_after = await effect_control_for_partition.effect_control_for_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_control_for_set', async () => {
      await effect_control_for_set.effect_control_for_set.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_control_for_set.effect_control_for_set.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_control_for_set.effect_control_for_set.exec({ as: alice })
      //      const res_after = await effect_control_for_set.effect_control_for_set.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_control_for_view', async () => {
      await effect_control_for_view.effect_control_for_view.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_control_for_view.effect_control_for_view.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_control_for_view.effect_control_for_view.exec({ as: alice })
      //      const res_after = await effect_control_for_view.effect_control_for_view.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_control_if', async () => {
      await effect_control_if.effect_control_if.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_control_if.effect_control_if.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_control_if.effect_control_if.exec({ as: alice })
      //      const res_after = await effect_control_if.effect_control_if.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_control_if_else', async () => {
      await effect_control_if_else.effect_control_if_else.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_control_if_else.effect_control_if_else.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_control_if_else.effect_control_if_else.exec({ as: alice })
      //      const res_after = await effect_control_if_else.effect_control_if_else.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_control_iter', async () => {
      await effect_control_iter.effect_control_iter.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_control_iter.effect_control_iter.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_control_iter.effect_control_iter.exec({ as: alice })
      //      const res_after = await effect_control_iter.effect_control_iter.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_control_iter_init', async () => {
      await effect_control_iter_init.effect_control_iter_init.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_control_iter_init.effect_control_iter_init.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_control_iter_init.effect_control_iter_init.exec({ as: alice })
      //      const res_after = await effect_control_iter_init.effect_control_iter_init.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_control_match_enum', async () => {
      await effect_control_match_enum.effect_control_match_enum.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_control_match_enum.effect_control_match_enum.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_control_match_enum.effect_control_match_enum.exec({ as: alice })
      //      const res_after = await effect_control_match_enum.effect_control_match_enum.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_control_match_list', async () => {
      await effect_control_match_list.effect_control_match_list.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_control_match_list.effect_control_match_list.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_control_match_list.effect_control_match_list.exec({ as: alice })
      //      const res_after = await effect_control_match_list.effect_control_match_list.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_control_match_option', async () => {
      await effect_control_match_option.effect_control_match_option.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_control_match_option.effect_control_match_option.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_control_match_option.effect_control_match_option.exec({ as: alice })
      //      const res_after = await effect_control_match_option.effect_control_match_option.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_control_match_or', async () => {
      await effect_control_match_or.effect_control_match_or.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_control_match_or.effect_control_match_or.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_control_match_or.effect_control_match_or.exec({ as: alice })
      //      const res_after = await effect_control_match_or.effect_control_match_or.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_control_matchwith', async () => {
      await effect_control_matchwith.effect_control_matchwith.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_control_matchwith.effect_control_matchwith.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_control_matchwith.effect_control_matchwith.exec({ as: alice })
      //      const res_after = await effect_control_matchwith.effect_control_matchwith.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_control_sequence', async () => {
      await effect_control_sequence.effect_control_sequence.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_control_sequence.effect_control_sequence.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_control_sequence.effect_control_sequence.exec({ as: alice })
      //      const res_after = await effect_control_sequence.effect_control_sequence.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_control_while', async () => {
      await effect_control_while.effect_control_while.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_control_while.effect_control_while.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_control_while.effect_control_while.exec({ as: alice })
      //      const res_after = await effect_control_while.effect_control_while.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_dofailif', async () => {
      await effect_dofailif.effect_dofailif.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_dofailif.effect_dofailif.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_dofailif.effect_dofailif.exec({ as: alice })
      //      const res_after = await effect_dofailif.effect_dofailif.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_dorequire', async () => {
      await effect_dorequire.effect_dorequire.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_dorequire.effect_dorequire.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_dorequire.effect_dorequire.exec({ as: alice })
      //      const res_after = await effect_dorequire.effect_dorequire.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_dorequire_not', async () => {
      await effect_dorequire_not.effect_dorequire_not.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_dorequire_not.effect_dorequire_not.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_dorequire_not.effect_dorequire_not.exec({ as: alice })
      //      const res_after = await effect_dorequire_not.effect_dorequire_not.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_fail', async () => {
      await effect_fail.effect_fail.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_fail.effect_fail.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_fail.effect_fail.exec({ as: alice })
      //      const res_after = await effect_fail.effect_fail.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_instruction_put_in_asset', async () => {
      await effect_instruction_put_in_asset.effect_instruction_put_in_asset.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_instruction_put_in_asset.effect_instruction_put_in_asset.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_instruction_put_in_asset.effect_instruction_put_in_asset.exec({ as: alice })
      //      const res_after = await effect_instruction_put_in_asset.effect_instruction_put_in_asset.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_add_aggregate', async () => {
      await effect_method_asset_add_aggregate.effect_method_asset_add_aggregate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_add_aggregate.effect_method_asset_add_aggregate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_add_aggregate.effect_method_asset_add_aggregate.exec({ as: alice })
      //      const res_after = await effect_method_asset_add_aggregate.effect_method_asset_add_aggregate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_add_asset', async () => {
      await effect_method_asset_add_asset.effect_method_asset_add_asset.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_add_asset.effect_method_asset_add_asset.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_add_asset.effect_method_asset_add_asset.exec({ as: alice })
      //      const res_after = await effect_method_asset_add_asset.effect_method_asset_add_asset.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_add_asset2', async () => {
      await effect_method_asset_add_asset2.effect_method_asset_add_asset2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_add_asset2.effect_method_asset_add_asset2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_add_asset2.effect_method_asset_add_asset2.exec({ as: alice })
      //      const res_after = await effect_method_asset_add_asset2.effect_method_asset_add_asset2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_add_asset_one_field', async () => {
      await effect_method_asset_add_asset_one_field.effect_method_asset_add_asset_one_field.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_add_asset_one_field.effect_method_asset_add_asset_one_field.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_add_asset_one_field.effect_method_asset_add_asset_one_field.exec({ as: alice })
      //      const res_after = await effect_method_asset_add_asset_one_field.effect_method_asset_add_asset_one_field.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_add_asset_with_aggregate', async () => {
      await effect_method_asset_add_asset_with_aggregate.effect_method_asset_add_asset_with_aggregate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_add_asset_with_aggregate.effect_method_asset_add_asset_with_aggregate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_add_asset_with_aggregate.effect_method_asset_add_asset_with_aggregate.exec({ as: alice })
      //      const res_after = await effect_method_asset_add_asset_with_aggregate.effect_method_asset_add_asset_with_aggregate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_add_asset_with_partition', async () => {
      await effect_method_asset_add_asset_with_partition.effect_method_asset_add_asset_with_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_add_asset_with_partition.effect_method_asset_add_asset_with_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_add_asset_with_partition.effect_method_asset_add_asset_with_partition.exec({ as: alice })
      //      const res_after = await effect_method_asset_add_asset_with_partition.effect_method_asset_add_asset_with_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_add_asset_with_partition_2', async () => {
      await effect_method_asset_add_asset_with_partition_2.effect_method_asset_add_asset_with_partition_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_add_asset_with_partition_2.effect_method_asset_add_asset_with_partition_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_add_asset_with_partition_2.effect_method_asset_add_asset_with_partition_2.exec({ as: alice })
      //      const res_after = await effect_method_asset_add_asset_with_partition_2.effect_method_asset_add_asset_with_partition_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_add_partition', async () => {
      await effect_method_asset_add_partition.effect_method_asset_add_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_add_partition.effect_method_asset_add_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_add_partition.effect_method_asset_add_partition.exec({ as: alice })
      //      const res_after = await effect_method_asset_add_partition.effect_method_asset_add_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_add_partition_one_field', async () => {
      await effect_method_asset_add_partition_one_field.effect_method_asset_add_partition_one_field.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_add_partition_one_field.effect_method_asset_add_partition_one_field.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_add_partition_one_field.effect_method_asset_add_partition_one_field.exec({ as: alice })
      //      const res_after = await effect_method_asset_add_partition_one_field.effect_method_asset_add_partition_one_field.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_addupdate', async () => {
      await effect_method_asset_addupdate.effect_method_asset_addupdate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_addupdate.effect_method_asset_addupdate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_addupdate.effect_method_asset_addupdate.exec({ as: alice })
      //      const res_after = await effect_method_asset_addupdate.effect_method_asset_addupdate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_addupdate_partition', async () => {
      await effect_method_asset_addupdate_partition.effect_method_asset_addupdate_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_addupdate_partition.effect_method_asset_addupdate_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_addupdate_partition.effect_method_asset_addupdate_partition.exec({ as: alice })
      //      const res_after = await effect_method_asset_addupdate_partition.effect_method_asset_addupdate_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_addupdate_with_add_aggregate', async () => {
      await effect_method_asset_addupdate_with_add_aggregate.effect_method_asset_addupdate_with_add_aggregate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_addupdate_with_add_aggregate.effect_method_asset_addupdate_with_add_aggregate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_addupdate_with_add_aggregate.effect_method_asset_addupdate_with_add_aggregate.exec({ as: alice })
      //      const res_after = await effect_method_asset_addupdate_with_add_aggregate.effect_method_asset_addupdate_with_add_aggregate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_addupdate_with_add_map', async () => {
      await effect_method_asset_addupdate_with_add_map.effect_method_asset_addupdate_with_add_map.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_addupdate_with_add_map.effect_method_asset_addupdate_with_add_map.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_addupdate_with_add_map.effect_method_asset_addupdate_with_add_map.exec({ as: alice })
      //      const res_after = await effect_method_asset_addupdate_with_add_map.effect_method_asset_addupdate_with_add_map.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_addupdate_with_add_map_var', async () => {
      await effect_method_asset_addupdate_with_add_map_var.effect_method_asset_addupdate_with_add_map_var.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_addupdate_with_add_map_var.effect_method_asset_addupdate_with_add_map_var.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_addupdate_with_add_map_var.effect_method_asset_addupdate_with_add_map_var.exec({ as: alice })
      //      const res_after = await effect_method_asset_addupdate_with_add_map_var.effect_method_asset_addupdate_with_add_map_var.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_addupdate_with_add_partition', async () => {
      await effect_method_asset_addupdate_with_add_partition.effect_method_asset_addupdate_with_add_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_addupdate_with_add_partition.effect_method_asset_addupdate_with_add_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_addupdate_with_add_partition.effect_method_asset_addupdate_with_add_partition.exec({ as: alice })
      //      const res_after = await effect_method_asset_addupdate_with_add_partition.effect_method_asset_addupdate_with_add_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_addupdate_with_add_set', async () => {
      await effect_method_asset_addupdate_with_add_set.effect_method_asset_addupdate_with_add_set.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_addupdate_with_add_set.effect_method_asset_addupdate_with_add_set.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_addupdate_with_add_set.effect_method_asset_addupdate_with_add_set.exec({ as: alice })
      //      const res_after = await effect_method_asset_addupdate_with_add_set.effect_method_asset_addupdate_with_add_set.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_addupdate_with_remove_map', async () => {
      await effect_method_asset_addupdate_with_remove_map.effect_method_asset_addupdate_with_remove_map.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_addupdate_with_remove_map.effect_method_asset_addupdate_with_remove_map.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_addupdate_with_remove_map.effect_method_asset_addupdate_with_remove_map.exec({ as: alice })
      //      const res_after = await effect_method_asset_addupdate_with_remove_map.effect_method_asset_addupdate_with_remove_map.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_addupdate_with_remove_set', async () => {
      await effect_method_asset_addupdate_with_remove_set.effect_method_asset_addupdate_with_remove_set.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_addupdate_with_remove_set.effect_method_asset_addupdate_with_remove_set.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_addupdate_with_remove_set.effect_method_asset_addupdate_with_remove_set.exec({ as: alice })
      //      const res_after = await effect_method_asset_addupdate_with_remove_set.effect_method_asset_addupdate_with_remove_set.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_addupdate_with_replace_aggregate', async () => {
      await effect_method_asset_addupdate_with_replace_aggregate.effect_method_asset_addupdate_with_replace_aggregate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_addupdate_with_replace_aggregate.effect_method_asset_addupdate_with_replace_aggregate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_addupdate_with_replace_aggregate.effect_method_asset_addupdate_with_replace_aggregate.exec({ as: alice })
      //      const res_after = await effect_method_asset_addupdate_with_replace_aggregate.effect_method_asset_addupdate_with_replace_aggregate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_addupdate_with_replace_partition', async () => {
      await effect_method_asset_addupdate_with_replace_partition.effect_method_asset_addupdate_with_replace_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_addupdate_with_replace_partition.effect_method_asset_addupdate_with_replace_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_addupdate_with_replace_partition.effect_method_asset_addupdate_with_replace_partition.exec({ as: alice })
      //      const res_after = await effect_method_asset_addupdate_with_replace_partition.effect_method_asset_addupdate_with_replace_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_big_map_0_put_remove_put', async () => {
      await effect_method_asset_big_map_0_put_remove_put.effect_method_asset_big_map_0_put_remove_put.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_big_map_0_put_remove_put.effect_method_asset_big_map_0_put_remove_put.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_big_map_0_put_remove_put.effect_method_asset_big_map_0_put_remove_put.exec({ as: alice })
      //      const res_after = await effect_method_asset_big_map_0_put_remove_put.effect_method_asset_big_map_0_put_remove_put.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_big_map_0_put_remove_remove', async () => {
      await effect_method_asset_big_map_0_put_remove_remove.effect_method_asset_big_map_0_put_remove_remove.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_big_map_0_put_remove_remove.effect_method_asset_big_map_0_put_remove_remove.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_big_map_0_put_remove_remove.effect_method_asset_big_map_0_put_remove_remove.exec({ as: alice })
      //      const res_after = await effect_method_asset_big_map_0_put_remove_remove.effect_method_asset_big_map_0_put_remove_remove.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_big_map_1_put_remove_put', async () => {
      await effect_method_asset_big_map_1_put_remove_put.effect_method_asset_big_map_1_put_remove_put.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_big_map_1_put_remove_put.effect_method_asset_big_map_1_put_remove_put.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_big_map_1_put_remove_put.effect_method_asset_big_map_1_put_remove_put.exec({ as: alice })
      //      const res_after = await effect_method_asset_big_map_1_put_remove_put.effect_method_asset_big_map_1_put_remove_put.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_big_map_1_put_remove_remove', async () => {
      await effect_method_asset_big_map_1_put_remove_remove.effect_method_asset_big_map_1_put_remove_remove.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_big_map_1_put_remove_remove.effect_method_asset_big_map_1_put_remove_remove.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_big_map_1_put_remove_remove.effect_method_asset_big_map_1_put_remove_remove.exec({ as: alice })
      //      const res_after = await effect_method_asset_big_map_1_put_remove_remove.effect_method_asset_big_map_1_put_remove_remove.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_big_map_2_put_remove_put', async () => {
      await effect_method_asset_big_map_2_put_remove_put.effect_method_asset_big_map_2_put_remove_put.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_big_map_2_put_remove_put.effect_method_asset_big_map_2_put_remove_put.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_big_map_2_put_remove_put.effect_method_asset_big_map_2_put_remove_put.exec({ as: alice })
      //      const res_after = await effect_method_asset_big_map_2_put_remove_put.effect_method_asset_big_map_2_put_remove_put.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_big_map_2_put_remove_remove', async () => {
      await effect_method_asset_big_map_2_put_remove_remove.effect_method_asset_big_map_2_put_remove_remove.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_big_map_2_put_remove_remove.effect_method_asset_big_map_2_put_remove_remove.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_big_map_2_put_remove_remove.effect_method_asset_big_map_2_put_remove_remove.exec({ as: alice })
      //      const res_after = await effect_method_asset_big_map_2_put_remove_remove.effect_method_asset_big_map_2_put_remove_remove.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_clear_view', async () => {
      await effect_method_asset_clear_view.effect_method_asset_clear_view.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_clear_view.effect_method_asset_clear_view.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_clear_view.effect_method_asset_clear_view.exec({ as: alice })
      //      const res_after = await effect_method_asset_clear_view.effect_method_asset_clear_view.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_clear_view_with_aggregate', async () => {
      await effect_method_asset_clear_view_with_aggregate.effect_method_asset_clear_view_with_aggregate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_clear_view_with_aggregate.effect_method_asset_clear_view_with_aggregate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_clear_view_with_aggregate.effect_method_asset_clear_view_with_aggregate.exec({ as: alice })
      //      const res_after = await effect_method_asset_clear_view_with_aggregate.effect_method_asset_clear_view_with_aggregate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_clear_view_with_partition', async () => {
      await effect_method_asset_clear_view_with_partition.effect_method_asset_clear_view_with_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_clear_view_with_partition.effect_method_asset_clear_view_with_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_clear_view_with_partition.effect_method_asset_clear_view_with_partition.exec({ as: alice })
      //      const res_after = await effect_method_asset_clear_view_with_partition.effect_method_asset_clear_view_with_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_map_0_put_remove_put', async () => {
      await effect_method_asset_map_0_put_remove_put.effect_method_asset_map_0_put_remove_put.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_map_0_put_remove_put.effect_method_asset_map_0_put_remove_put.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_map_0_put_remove_put.effect_method_asset_map_0_put_remove_put.exec({ as: alice })
      //      const res_after = await effect_method_asset_map_0_put_remove_put.effect_method_asset_map_0_put_remove_put.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_map_0_put_remove_remove', async () => {
      await effect_method_asset_map_0_put_remove_remove.effect_method_asset_map_0_put_remove_remove.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_map_0_put_remove_remove.effect_method_asset_map_0_put_remove_remove.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_map_0_put_remove_remove.effect_method_asset_map_0_put_remove_remove.exec({ as: alice })
      //      const res_after = await effect_method_asset_map_0_put_remove_remove.effect_method_asset_map_0_put_remove_remove.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_map_1_put_remove_put', async () => {
      await effect_method_asset_map_1_put_remove_put.effect_method_asset_map_1_put_remove_put.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_map_1_put_remove_put.effect_method_asset_map_1_put_remove_put.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_map_1_put_remove_put.effect_method_asset_map_1_put_remove_put.exec({ as: alice })
      //      const res_after = await effect_method_asset_map_1_put_remove_put.effect_method_asset_map_1_put_remove_put.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_map_1_put_remove_remove', async () => {
      await effect_method_asset_map_1_put_remove_remove.effect_method_asset_map_1_put_remove_remove.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_map_1_put_remove_remove.effect_method_asset_map_1_put_remove_remove.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_map_1_put_remove_remove.effect_method_asset_map_1_put_remove_remove.exec({ as: alice })
      //      const res_after = await effect_method_asset_map_1_put_remove_remove.effect_method_asset_map_1_put_remove_remove.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_map_2_put_remove_put', async () => {
      await effect_method_asset_map_2_put_remove_put.effect_method_asset_map_2_put_remove_put.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_map_2_put_remove_put.effect_method_asset_map_2_put_remove_put.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_map_2_put_remove_put.effect_method_asset_map_2_put_remove_put.exec({ as: alice })
      //      const res_after = await effect_method_asset_map_2_put_remove_put.effect_method_asset_map_2_put_remove_put.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_map_2_put_remove_remove', async () => {
      await effect_method_asset_map_2_put_remove_remove.effect_method_asset_map_2_put_remove_remove.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_map_2_put_remove_remove.effect_method_asset_map_2_put_remove_remove.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_map_2_put_remove_remove.effect_method_asset_map_2_put_remove_remove.exec({ as: alice })
      //      const res_after = await effect_method_asset_map_2_put_remove_remove.effect_method_asset_map_2_put_remove_remove.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_remove_aggregate', async () => {
      await effect_method_asset_remove_aggregate.effect_method_asset_remove_aggregate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_remove_aggregate.effect_method_asset_remove_aggregate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_remove_aggregate.effect_method_asset_remove_aggregate.exec({ as: alice })
      //      const res_after = await effect_method_asset_remove_aggregate.effect_method_asset_remove_aggregate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_remove_all_aggregate', async () => {
      await effect_method_asset_remove_all_aggregate.effect_method_asset_remove_all_aggregate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_remove_all_aggregate.effect_method_asset_remove_all_aggregate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_remove_all_aggregate.effect_method_asset_remove_all_aggregate.exec({ as: alice })
      //      const res_after = await effect_method_asset_remove_all_aggregate.effect_method_asset_remove_all_aggregate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_remove_all_asset_one_field', async () => {
      await effect_method_asset_remove_all_asset_one_field.effect_method_asset_remove_all_asset_one_field.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_remove_all_asset_one_field.effect_method_asset_remove_all_asset_one_field.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_remove_all_asset_one_field.effect_method_asset_remove_all_asset_one_field.exec({ as: alice })
      //      const res_after = await effect_method_asset_remove_all_asset_one_field.effect_method_asset_remove_all_asset_one_field.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_remove_all_asset_with_aggregate', async () => {
      await effect_method_asset_remove_all_asset_with_aggregate.effect_method_asset_remove_all_asset_with_aggregate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_remove_all_asset_with_aggregate.effect_method_asset_remove_all_asset_with_aggregate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_remove_all_asset_with_aggregate.effect_method_asset_remove_all_asset_with_aggregate.exec({ as: alice })
      //      const res_after = await effect_method_asset_remove_all_asset_with_aggregate.effect_method_asset_remove_all_asset_with_aggregate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_remove_all_asset_with_partition', async () => {
      await effect_method_asset_remove_all_asset_with_partition.effect_method_asset_remove_all_asset_with_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_remove_all_asset_with_partition.effect_method_asset_remove_all_asset_with_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_remove_all_asset_with_partition.effect_method_asset_remove_all_asset_with_partition.exec({ as: alice })
      //      const res_after = await effect_method_asset_remove_all_asset_with_partition.effect_method_asset_remove_all_asset_with_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_remove_all_collection', async () => {
      await effect_method_asset_remove_all_collection.effect_method_asset_remove_all_collection.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_remove_all_collection.effect_method_asset_remove_all_collection.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_remove_all_collection.effect_method_asset_remove_all_collection.exec({ as: alice })
      //      const res_after = await effect_method_asset_remove_all_collection.effect_method_asset_remove_all_collection.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_remove_asset', async () => {
      await effect_method_asset_remove_asset.effect_method_asset_remove_asset.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_remove_asset.effect_method_asset_remove_asset.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_remove_asset.effect_method_asset_remove_asset.exec({ as: alice })
      //      const res_after = await effect_method_asset_remove_asset.effect_method_asset_remove_asset.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_remove_asset2', async () => {
      await effect_method_asset_remove_asset2.effect_method_asset_remove_asset2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_remove_asset2.effect_method_asset_remove_asset2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_remove_asset2.effect_method_asset_remove_asset2.exec({ as: alice })
      //      const res_after = await effect_method_asset_remove_asset2.effect_method_asset_remove_asset2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_remove_asset_one_field', async () => {
      await effect_method_asset_remove_asset_one_field.effect_method_asset_remove_asset_one_field.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_remove_asset_one_field.effect_method_asset_remove_asset_one_field.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_remove_asset_one_field.effect_method_asset_remove_asset_one_field.exec({ as: alice })
      //      const res_after = await effect_method_asset_remove_asset_one_field.effect_method_asset_remove_asset_one_field.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_remove_asset_with_aggregate', async () => {
      await effect_method_asset_remove_asset_with_aggregate.effect_method_asset_remove_asset_with_aggregate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_remove_asset_with_aggregate.effect_method_asset_remove_asset_with_aggregate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_remove_asset_with_aggregate.effect_method_asset_remove_asset_with_aggregate.exec({ as: alice })
      //      const res_after = await effect_method_asset_remove_asset_with_aggregate.effect_method_asset_remove_asset_with_aggregate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_remove_asset_with_partition', async () => {
      await effect_method_asset_remove_asset_with_partition.effect_method_asset_remove_asset_with_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_remove_asset_with_partition.effect_method_asset_remove_asset_with_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_remove_asset_with_partition.effect_method_asset_remove_asset_with_partition.exec({ as: alice })
      //      const res_after = await effect_method_asset_remove_asset_with_partition.effect_method_asset_remove_asset_with_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_remove_asset_with_partition_2', async () => {
      await effect_method_asset_remove_asset_with_partition_2.effect_method_asset_remove_asset_with_partition_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_remove_asset_with_partition_2.effect_method_asset_remove_asset_with_partition_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_remove_asset_with_partition_2.effect_method_asset_remove_asset_with_partition_2.exec({ as: alice })
      //      const res_after = await effect_method_asset_remove_asset_with_partition_2.effect_method_asset_remove_asset_with_partition_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_remove_partition', async () => {
      await effect_method_asset_remove_partition.effect_method_asset_remove_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_remove_partition.effect_method_asset_remove_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_remove_partition.effect_method_asset_remove_partition.exec({ as: alice })
      //      const res_after = await effect_method_asset_remove_partition.effect_method_asset_remove_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_removeall_aggregate', async () => {
      await effect_method_asset_removeall_aggregate.effect_method_asset_removeall_aggregate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_removeall_aggregate.effect_method_asset_removeall_aggregate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_removeall_aggregate.effect_method_asset_removeall_aggregate.exec({ as: alice })
      //      const res_after = await effect_method_asset_removeall_aggregate.effect_method_asset_removeall_aggregate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_removeall_partition', async () => {
      await effect_method_asset_removeall_partition.effect_method_asset_removeall_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_removeall_partition.effect_method_asset_removeall_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_removeall_partition.effect_method_asset_removeall_partition.exec({ as: alice })
      //      const res_after = await effect_method_asset_removeall_partition.effect_method_asset_removeall_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_removeif_aggregate', async () => {
      await effect_method_asset_removeif_aggregate.effect_method_asset_removeif_aggregate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_removeif_aggregate.effect_method_asset_removeif_aggregate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_removeif_aggregate.effect_method_asset_removeif_aggregate.exec({ as: alice })
      //      const res_after = await effect_method_asset_removeif_aggregate.effect_method_asset_removeif_aggregate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_removeif_collection', async () => {
      await effect_method_asset_removeif_collection.effect_method_asset_removeif_collection.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_removeif_collection.effect_method_asset_removeif_collection.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_removeif_collection.effect_method_asset_removeif_collection.exec({ as: alice })
      //      const res_after = await effect_method_asset_removeif_collection.effect_method_asset_removeif_collection.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_removeif_collection_with_aggregate', async () => {
      await effect_method_asset_removeif_collection_with_aggregate.effect_method_asset_removeif_collection_with_aggregate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_removeif_collection_with_aggregate.effect_method_asset_removeif_collection_with_aggregate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_removeif_collection_with_aggregate.effect_method_asset_removeif_collection_with_aggregate.exec({ as: alice })
      //      const res_after = await effect_method_asset_removeif_collection_with_aggregate.effect_method_asset_removeif_collection_with_aggregate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_removeif_collection_with_partition', async () => {
      await effect_method_asset_removeif_collection_with_partition.effect_method_asset_removeif_collection_with_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_removeif_collection_with_partition.effect_method_asset_removeif_collection_with_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_removeif_collection_with_partition.effect_method_asset_removeif_collection_with_partition.exec({ as: alice })
      //      const res_after = await effect_method_asset_removeif_collection_with_partition.effect_method_asset_removeif_collection_with_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_removeif_partition', async () => {
      await effect_method_asset_removeif_partition.effect_method_asset_removeif_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_removeif_partition.effect_method_asset_removeif_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_removeif_partition.effect_method_asset_removeif_partition.exec({ as: alice })
      //      const res_after = await effect_method_asset_removeif_partition.effect_method_asset_removeif_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_update', async () => {
      await effect_method_asset_update.effect_method_asset_update.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_update.effect_method_asset_update.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_update.effect_method_asset_update.exec({ as: alice })
      //      const res_after = await effect_method_asset_update.effect_method_asset_update.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_update_all_coll_1', async () => {
      await effect_method_asset_update_all_coll_1.effect_method_asset_update_all_coll_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_update_all_coll_1.effect_method_asset_update_all_coll_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_update_all_coll_1.effect_method_asset_update_all_coll_1.exec({ as: alice })
      //      const res_after = await effect_method_asset_update_all_coll_1.effect_method_asset_update_all_coll_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_update_all_coll_2', async () => {
      await effect_method_asset_update_all_coll_2.effect_method_asset_update_all_coll_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_update_all_coll_2.effect_method_asset_update_all_coll_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_update_all_coll_2.effect_method_asset_update_all_coll_2.exec({ as: alice })
      //      const res_after = await effect_method_asset_update_all_coll_2.effect_method_asset_update_all_coll_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_update_all_view_1', async () => {
      await effect_method_asset_update_all_view_1.effect_method_asset_update_all_view_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_update_all_view_1.effect_method_asset_update_all_view_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_update_all_view_1.effect_method_asset_update_all_view_1.exec({ as: alice })
      //      const res_after = await effect_method_asset_update_all_view_1.effect_method_asset_update_all_view_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_update_all_view_2', async () => {
      await effect_method_asset_update_all_view_2.effect_method_asset_update_all_view_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_update_all_view_2.effect_method_asset_update_all_view_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_update_all_view_2.effect_method_asset_update_all_view_2.exec({ as: alice })
      //      const res_after = await effect_method_asset_update_all_view_2.effect_method_asset_update_all_view_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_update_with_add_aggregate', async () => {
      await effect_method_asset_update_with_add_aggregate.effect_method_asset_update_with_add_aggregate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_update_with_add_aggregate.effect_method_asset_update_with_add_aggregate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_update_with_add_aggregate.effect_method_asset_update_with_add_aggregate.exec({ as: alice })
      //      const res_after = await effect_method_asset_update_with_add_aggregate.effect_method_asset_update_with_add_aggregate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_update_with_add_map', async () => {
      await effect_method_asset_update_with_add_map.effect_method_asset_update_with_add_map.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_update_with_add_map.effect_method_asset_update_with_add_map.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_update_with_add_map.effect_method_asset_update_with_add_map.exec({ as: alice })
      //      const res_after = await effect_method_asset_update_with_add_map.effect_method_asset_update_with_add_map.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_update_with_add_partition', async () => {
      await effect_method_asset_update_with_add_partition.effect_method_asset_update_with_add_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_update_with_add_partition.effect_method_asset_update_with_add_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_update_with_add_partition.effect_method_asset_update_with_add_partition.exec({ as: alice })
      //      const res_after = await effect_method_asset_update_with_add_partition.effect_method_asset_update_with_add_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_update_with_add_set', async () => {
      await effect_method_asset_update_with_add_set.effect_method_asset_update_with_add_set.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_update_with_add_set.effect_method_asset_update_with_add_set.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_update_with_add_set.effect_method_asset_update_with_add_set.exec({ as: alice })
      //      const res_after = await effect_method_asset_update_with_add_set.effect_method_asset_update_with_add_set.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_update_with_map', async () => {
      await effect_method_asset_update_with_map.effect_method_asset_update_with_map.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_update_with_map.effect_method_asset_update_with_map.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_update_with_map.effect_method_asset_update_with_map.exec({ as: alice })
      //      const res_after = await effect_method_asset_update_with_map.effect_method_asset_update_with_map.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_update_with_remove_aggregate', async () => {
      await effect_method_asset_update_with_remove_aggregate.effect_method_asset_update_with_remove_aggregate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_update_with_remove_aggregate.effect_method_asset_update_with_remove_aggregate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_update_with_remove_aggregate.effect_method_asset_update_with_remove_aggregate.exec({ as: alice })
      //      const res_after = await effect_method_asset_update_with_remove_aggregate.effect_method_asset_update_with_remove_aggregate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_update_with_remove_map', async () => {
      await effect_method_asset_update_with_remove_map.effect_method_asset_update_with_remove_map.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_update_with_remove_map.effect_method_asset_update_with_remove_map.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_update_with_remove_map.effect_method_asset_update_with_remove_map.exec({ as: alice })
      //      const res_after = await effect_method_asset_update_with_remove_map.effect_method_asset_update_with_remove_map.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_update_with_remove_partition', async () => {
      await effect_method_asset_update_with_remove_partition.effect_method_asset_update_with_remove_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_update_with_remove_partition.effect_method_asset_update_with_remove_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_update_with_remove_partition.effect_method_asset_update_with_remove_partition.exec({ as: alice })
      //      const res_after = await effect_method_asset_update_with_remove_partition.effect_method_asset_update_with_remove_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_update_with_remove_set', async () => {
      await effect_method_asset_update_with_remove_set.effect_method_asset_update_with_remove_set.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_update_with_remove_set.effect_method_asset_update_with_remove_set.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_update_with_remove_set.effect_method_asset_update_with_remove_set.exec({ as: alice })
      //      const res_after = await effect_method_asset_update_with_remove_set.effect_method_asset_update_with_remove_set.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_update_with_replace_aggregate', async () => {
      await effect_method_asset_update_with_replace_aggregate.effect_method_asset_update_with_replace_aggregate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_update_with_replace_aggregate.effect_method_asset_update_with_replace_aggregate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_update_with_replace_aggregate.effect_method_asset_update_with_replace_aggregate.exec({ as: alice })
      //      const res_after = await effect_method_asset_update_with_replace_aggregate.effect_method_asset_update_with_replace_aggregate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_update_with_replace_partition', async () => {
      await effect_method_asset_update_with_replace_partition.effect_method_asset_update_with_replace_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_update_with_replace_partition.effect_method_asset_update_with_replace_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_update_with_replace_partition.effect_method_asset_update_with_replace_partition.exec({ as: alice })
      //      const res_after = await effect_method_asset_update_with_replace_partition.effect_method_asset_update_with_replace_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_method_asset_update_with_set', async () => {
      await effect_method_asset_update_with_set.effect_method_asset_update_with_set.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_method_asset_update_with_set.effect_method_asset_update_with_set.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_method_asset_update_with_set.effect_method_asset_update_with_set.exec({ as: alice })
      //      const res_after = await effect_method_asset_update_with_set.effect_method_asset_update_with_set.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_transfer_contract', async () => {
      await effect_transfer_contract.effect_transfer_contract.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_transfer_contract.effect_transfer_contract.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_transfer_contract.effect_transfer_contract.exec({ as: alice })
      //      const res_after = await effect_transfer_contract.effect_transfer_contract.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('effect_transfer_simple', async () => {
      await effect_transfer_simple.effect_transfer_simple.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await effect_transfer_simple.effect_transfer_simple.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await effect_transfer_simple.effect_transfer_simple.exec({ as: alice })
      //      const res_after = await effect_transfer_simple.effect_transfer_simple.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('entry_inspector', async () => {
      await entry_inspector.entry_inspector.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await entry_inspector.entry_inspector.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await entry_inspector.entry_inspector.exec({ as: alice })
      //      const res_after = await entry_inspector.entry_inspector.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('entry_section_called_by_otherwise', async () => {
      await entry_section_called_by_otherwise.entry_section_called_by_otherwise.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await entry_section_called_by_otherwise.entry_section_called_by_otherwise.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await entry_section_called_by_otherwise.entry_section_called_by_otherwise.exec({ as: alice })
      //      const res_after = await entry_section_called_by_otherwise.entry_section_called_by_otherwise.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('entry_section_no_transfer_otherwise', async () => {
      await entry_section_no_transfer_otherwise.entry_section_no_transfer_otherwise.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await entry_section_no_transfer_otherwise.entry_section_no_transfer_otherwise.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await entry_section_no_transfer_otherwise.entry_section_no_transfer_otherwise.exec({ as: alice })
      //      const res_after = await entry_section_no_transfer_otherwise.entry_section_no_transfer_otherwise.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('entry_section_sourced_by_otherwise', async () => {
      await entry_section_sourced_by_otherwise.entry_section_sourced_by_otherwise.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await entry_section_sourced_by_otherwise.entry_section_sourced_by_otherwise.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await entry_section_sourced_by_otherwise.entry_section_sourced_by_otherwise.exec({ as: alice })
      //      const res_after = await entry_section_sourced_by_otherwise.entry_section_sourced_by_otherwise.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('entry_section_state_is_otherwise', async () => {
      await entry_section_state_is_otherwise.entry_section_state_is_otherwise.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await entry_section_state_is_otherwise.entry_section_state_is_otherwise.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await entry_section_state_is_otherwise.entry_section_state_is_otherwise.exec({ as: alice })
      //      const res_after = await entry_section_state_is_otherwise.entry_section_state_is_otherwise.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('entry_token', async () => {
      await entry_token.entry_token.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await entry_token.entry_token.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await entry_token.entry_token.exec({ as: alice })
      //      const res_after = await entry_token.entry_token.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('entry_without_effect', async () => {
      await entry_without_effect.entry_without_effect.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await entry_without_effect.entry_without_effect.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await entry_without_effect.entry_without_effect.exec({ as: alice })
      //      const res_after = await entry_without_effect.entry_without_effect.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('enum_all', async () => {
      await enum_all.enum_all.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await enum_all.enum_all.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await enum_all.enum_all.exec({ as: alice })
      //      const res_after = await enum_all.enum_all.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('enum_key', async () => {
      await enum_key.enum_key.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await enum_key.enum_key.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await enum_key.enum_key.exec({ as: alice })
      //      const res_after = await enum_key.enum_key.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('enum_with_args', async () => {
      await enum_with_args.enum_with_args.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await enum_with_args.enum_with_args.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await enum_with_args.enum_with_args.exec({ as: alice })
      //      const res_after = await enum_with_args.enum_with_args.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('enum_with_args_multi', async () => {
      await enum_with_args_multi.enum_with_args_multi.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await enum_with_args_multi.enum_with_args_multi.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await enum_with_args_multi.enum_with_args_multi.exec({ as: alice })
      //      const res_after = await enum_with_args_multi.enum_with_args_multi.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('enum_without_args', async () => {
      await enum_without_args.enum_without_args.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await enum_without_args.enum_without_args.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await enum_without_args.enum_without_args.exec({ as: alice })
      //      const res_after = await enum_without_args.enum_without_args.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('event_all', async () => {
      await event_all.event_all.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await event_all.event_all.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await event_all.event_all.exec({ as: alice })
      //      const res_after = await event_all.event_all.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('event_dup', async () => {
      await event_dup.event_dup.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await event_dup.event_dup.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await event_dup.event_dup.exec({ as: alice })
      //      const res_after = await event_dup.event_dup.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('event_multi', async () => {
      await event_multi.event_multi.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await event_multi.event_multi.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await event_multi.event_multi.exec({ as: alice })
      //      const res_after = await event_multi.event_multi.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('event_simple', async () => {
      await event_simple.event_simple.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await event_simple.event_simple.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await event_simple.event_simple.exec({ as: alice })
      //      const res_after = await event_simple.event_simple.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('event_single', async () => {
      await event_single.event_single.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await event_single.event_single.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await event_single.event_single.exec({ as: alice })
      //      const res_after = await event_single.event_single.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('exec_letin', async () => {
      await exec_letin.exec_letin.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await exec_letin.exec_letin.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await exec_letin.exec_letin.exec({ as: alice })
      //      const res_after = await exec_letin.exec_letin.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_access_asset_field', async () => {
      await expr_access_asset_field.expr_access_asset_field.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_access_asset_field.expr_access_asset_field.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_access_asset_field.expr_access_asset_field.exec({ as: alice })
      //      const res_after = await expr_access_asset_field.expr_access_asset_field.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_3wc_nat_nat', async () => {
      await expr_arith_3wc_nat_nat.expr_arith_3wc_nat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_3wc_nat_nat.expr_arith_3wc_nat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_3wc_nat_nat.expr_arith_3wc_nat_nat.exec({ as: alice })
      //      const res_after = await expr_arith_3wc_nat_nat.expr_arith_3wc_nat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_and_bool_bool', async () => {
      await expr_arith_and_bool_bool.expr_arith_and_bool_bool.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_and_bool_bool.expr_arith_and_bool_bool.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_and_bool_bool.expr_arith_and_bool_bool.exec({ as: alice })
      //      const res_after = await expr_arith_and_bool_bool.expr_arith_and_bool_bool.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_and_int_nat', async () => {
      await expr_arith_and_int_nat.expr_arith_and_int_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_and_int_nat.expr_arith_and_int_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_and_int_nat.expr_arith_and_int_nat.exec({ as: alice })
      //      const res_after = await expr_arith_and_int_nat.expr_arith_and_int_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_and_nat_nat', async () => {
      await expr_arith_and_nat_nat.expr_arith_and_nat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_and_nat_nat.expr_arith_and_nat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_and_nat_nat.expr_arith_and_nat_nat.exec({ as: alice })
      //      const res_after = await expr_arith_and_nat_nat.expr_arith_and_nat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_div_dur_dur', async () => {
      await expr_arith_div_dur_dur.expr_arith_div_dur_dur.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_div_dur_dur.expr_arith_div_dur_dur.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_div_dur_dur.expr_arith_div_dur_dur.exec({ as: alice })
      //      const res_after = await expr_arith_div_dur_dur.expr_arith_div_dur_dur.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_div_int_int', async () => {
      await expr_arith_div_int_int.expr_arith_div_int_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_div_int_int.expr_arith_div_int_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_div_int_int.expr_arith_div_int_int.exec({ as: alice })
      //      const res_after = await expr_arith_div_int_int.expr_arith_div_int_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_div_int_nat', async () => {
      await expr_arith_div_int_nat.expr_arith_div_int_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_div_int_nat.expr_arith_div_int_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_div_int_nat.expr_arith_div_int_nat.exec({ as: alice })
      //      const res_after = await expr_arith_div_int_nat.expr_arith_div_int_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_div_int_rat', async () => {
      await expr_arith_div_int_rat.expr_arith_div_int_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_div_int_rat.expr_arith_div_int_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_div_int_rat.expr_arith_div_int_rat.exec({ as: alice })
      //      const res_after = await expr_arith_div_int_rat.expr_arith_div_int_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_div_nat_int', async () => {
      await expr_arith_div_nat_int.expr_arith_div_nat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_div_nat_int.expr_arith_div_nat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_div_nat_int.expr_arith_div_nat_int.exec({ as: alice })
      //      const res_after = await expr_arith_div_nat_int.expr_arith_div_nat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_div_nat_nat', async () => {
      await expr_arith_div_nat_nat.expr_arith_div_nat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_div_nat_nat.expr_arith_div_nat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_div_nat_nat.expr_arith_div_nat_nat.exec({ as: alice })
      //      const res_after = await expr_arith_div_nat_nat.expr_arith_div_nat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_div_nat_rat', async () => {
      await expr_arith_div_nat_rat.expr_arith_div_nat_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_div_nat_rat.expr_arith_div_nat_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_div_nat_rat.expr_arith_div_nat_rat.exec({ as: alice })
      //      const res_after = await expr_arith_div_nat_rat.expr_arith_div_nat_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_div_rat_int', async () => {
      await expr_arith_div_rat_int.expr_arith_div_rat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_div_rat_int.expr_arith_div_rat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_div_rat_int.expr_arith_div_rat_int.exec({ as: alice })
      //      const res_after = await expr_arith_div_rat_int.expr_arith_div_rat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_div_rat_nat', async () => {
      await expr_arith_div_rat_nat.expr_arith_div_rat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_div_rat_nat.expr_arith_div_rat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_div_rat_nat.expr_arith_div_rat_nat.exec({ as: alice })
      //      const res_after = await expr_arith_div_rat_nat.expr_arith_div_rat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_div_rat_rat', async () => {
      await expr_arith_div_rat_rat.expr_arith_div_rat_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_div_rat_rat.expr_arith_div_rat_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_div_rat_rat.expr_arith_div_rat_rat.exec({ as: alice })
      //      const res_after = await expr_arith_div_rat_rat.expr_arith_div_rat_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_div_tez_tez', async () => {
      await expr_arith_div_tez_tez.expr_arith_div_tez_tez.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_div_tez_tez.expr_arith_div_tez_tez.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_div_tez_tez.expr_arith_div_tez_tez.exec({ as: alice })
      //      const res_after = await expr_arith_div_tez_tez.expr_arith_div_tez_tez.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_divmod_int_int', async () => {
      await expr_arith_divmod_int_int.expr_arith_divmod_int_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_divmod_int_int.expr_arith_divmod_int_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_divmod_int_int.expr_arith_divmod_int_int.exec({ as: alice })
      //      const res_after = await expr_arith_divmod_int_int.expr_arith_divmod_int_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_divmod_int_nat', async () => {
      await expr_arith_divmod_int_nat.expr_arith_divmod_int_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_divmod_int_nat.expr_arith_divmod_int_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_divmod_int_nat.expr_arith_divmod_int_nat.exec({ as: alice })
      //      const res_after = await expr_arith_divmod_int_nat.expr_arith_divmod_int_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_divmod_nat_int', async () => {
      await expr_arith_divmod_nat_int.expr_arith_divmod_nat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_divmod_nat_int.expr_arith_divmod_nat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_divmod_nat_int.expr_arith_divmod_nat_int.exec({ as: alice })
      //      const res_after = await expr_arith_divmod_nat_int.expr_arith_divmod_nat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_divmod_nat_nat', async () => {
      await expr_arith_divmod_nat_nat.expr_arith_divmod_nat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_divmod_nat_nat.expr_arith_divmod_nat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_divmod_nat_nat.expr_arith_divmod_nat_nat.exec({ as: alice })
      //      const res_after = await expr_arith_divmod_nat_nat.expr_arith_divmod_nat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_divmod_tez_nat', async () => {
      await expr_arith_divmod_tez_nat.expr_arith_divmod_tez_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_divmod_tez_nat.expr_arith_divmod_tez_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_divmod_tez_nat.expr_arith_divmod_tez_nat.exec({ as: alice })
      //      const res_after = await expr_arith_divmod_tez_nat.expr_arith_divmod_tez_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_divmod_tez_tez', async () => {
      await expr_arith_divmod_tez_tez.expr_arith_divmod_tez_tez.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_divmod_tez_tez.expr_arith_divmod_tez_tez.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_divmod_tez_tez.expr_arith_divmod_tez_tez.exec({ as: alice })
      //      const res_after = await expr_arith_divmod_tez_tez.expr_arith_divmod_tez_tez.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_ediv_dur_dur', async () => {
      await expr_arith_ediv_dur_dur.expr_arith_ediv_dur_dur.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_ediv_dur_dur.expr_arith_ediv_dur_dur.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_ediv_dur_dur.expr_arith_ediv_dur_dur.exec({ as: alice })
      //      const res_after = await expr_arith_ediv_dur_dur.expr_arith_ediv_dur_dur.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_ediv_dur_int', async () => {
      await expr_arith_ediv_dur_int.expr_arith_ediv_dur_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_ediv_dur_int.expr_arith_ediv_dur_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_ediv_dur_int.expr_arith_ediv_dur_int.exec({ as: alice })
      //      const res_after = await expr_arith_ediv_dur_int.expr_arith_ediv_dur_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_ediv_dur_nat', async () => {
      await expr_arith_ediv_dur_nat.expr_arith_ediv_dur_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_ediv_dur_nat.expr_arith_ediv_dur_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_ediv_dur_nat.expr_arith_ediv_dur_nat.exec({ as: alice })
      //      const res_after = await expr_arith_ediv_dur_nat.expr_arith_ediv_dur_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_ediv_int_int', async () => {
      await expr_arith_ediv_int_int.expr_arith_ediv_int_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_ediv_int_int.expr_arith_ediv_int_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_ediv_int_int.expr_arith_ediv_int_int.exec({ as: alice })
      //      const res_after = await expr_arith_ediv_int_int.expr_arith_ediv_int_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_ediv_int_nat', async () => {
      await expr_arith_ediv_int_nat.expr_arith_ediv_int_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_ediv_int_nat.expr_arith_ediv_int_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_ediv_int_nat.expr_arith_ediv_int_nat.exec({ as: alice })
      //      const res_after = await expr_arith_ediv_int_nat.expr_arith_ediv_int_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_ediv_nat_int', async () => {
      await expr_arith_ediv_nat_int.expr_arith_ediv_nat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_ediv_nat_int.expr_arith_ediv_nat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_ediv_nat_int.expr_arith_ediv_nat_int.exec({ as: alice })
      //      const res_after = await expr_arith_ediv_nat_int.expr_arith_ediv_nat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_ediv_nat_nat', async () => {
      await expr_arith_ediv_nat_nat.expr_arith_ediv_nat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_ediv_nat_nat.expr_arith_ediv_nat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_ediv_nat_nat.expr_arith_ediv_nat_nat.exec({ as: alice })
      //      const res_after = await expr_arith_ediv_nat_nat.expr_arith_ediv_nat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_ediv_tez_nat', async () => {
      await expr_arith_ediv_tez_nat.expr_arith_ediv_tez_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_ediv_tez_nat.expr_arith_ediv_tez_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_ediv_tez_nat.expr_arith_ediv_tez_nat.exec({ as: alice })
      //      const res_after = await expr_arith_ediv_tez_nat.expr_arith_ediv_tez_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_ediv_tez_tez', async () => {
      await expr_arith_ediv_tez_tez.expr_arith_ediv_tez_tez.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_ediv_tez_tez.expr_arith_ediv_tez_tez.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_ediv_tez_tez.expr_arith_ediv_tez_tez.exec({ as: alice })
      //      const res_after = await expr_arith_ediv_tez_tez.expr_arith_ediv_tez_tez.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_lsl_nat_nat', async () => {
      await expr_arith_lsl_nat_nat.expr_arith_lsl_nat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_lsl_nat_nat.expr_arith_lsl_nat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_lsl_nat_nat.expr_arith_lsl_nat_nat.exec({ as: alice })
      //      const res_after = await expr_arith_lsl_nat_nat.expr_arith_lsl_nat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_lsr_nat_nat', async () => {
      await expr_arith_lsr_nat_nat.expr_arith_lsr_nat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_lsr_nat_nat.expr_arith_lsr_nat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_lsr_nat_nat.expr_arith_lsr_nat_nat.exec({ as: alice })
      //      const res_after = await expr_arith_lsr_nat_nat.expr_arith_lsr_nat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_minus_date_date', async () => {
      await expr_arith_minus_date_date.expr_arith_minus_date_date.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_minus_date_date.expr_arith_minus_date_date.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_minus_date_date.expr_arith_minus_date_date.exec({ as: alice })
      //      const res_after = await expr_arith_minus_date_date.expr_arith_minus_date_date.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_minus_date_date_neg', async () => {
      await expr_arith_minus_date_date_neg.expr_arith_minus_date_date_neg.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_minus_date_date_neg.expr_arith_minus_date_date_neg.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_minus_date_date_neg.expr_arith_minus_date_date_neg.exec({ as: alice })
      //      const res_after = await expr_arith_minus_date_date_neg.expr_arith_minus_date_date_neg.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_minus_date_dur', async () => {
      await expr_arith_minus_date_dur.expr_arith_minus_date_dur.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_minus_date_dur.expr_arith_minus_date_dur.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_minus_date_dur.expr_arith_minus_date_dur.exec({ as: alice })
      //      const res_after = await expr_arith_minus_date_dur.expr_arith_minus_date_dur.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_minus_dur_dur', async () => {
      await expr_arith_minus_dur_dur.expr_arith_minus_dur_dur.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_minus_dur_dur.expr_arith_minus_dur_dur.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_minus_dur_dur.expr_arith_minus_dur_dur.exec({ as: alice })
      //      const res_after = await expr_arith_minus_dur_dur.expr_arith_minus_dur_dur.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_minus_int_int', async () => {
      await expr_arith_minus_int_int.expr_arith_minus_int_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_minus_int_int.expr_arith_minus_int_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_minus_int_int.expr_arith_minus_int_int.exec({ as: alice })
      //      const res_after = await expr_arith_minus_int_int.expr_arith_minus_int_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_minus_int_nat', async () => {
      await expr_arith_minus_int_nat.expr_arith_minus_int_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_minus_int_nat.expr_arith_minus_int_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_minus_int_nat.expr_arith_minus_int_nat.exec({ as: alice })
      //      const res_after = await expr_arith_minus_int_nat.expr_arith_minus_int_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_minus_int_rat', async () => {
      await expr_arith_minus_int_rat.expr_arith_minus_int_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_minus_int_rat.expr_arith_minus_int_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_minus_int_rat.expr_arith_minus_int_rat.exec({ as: alice })
      //      const res_after = await expr_arith_minus_int_rat.expr_arith_minus_int_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_minus_nat_int', async () => {
      await expr_arith_minus_nat_int.expr_arith_minus_nat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_minus_nat_int.expr_arith_minus_nat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_minus_nat_int.expr_arith_minus_nat_int.exec({ as: alice })
      //      const res_after = await expr_arith_minus_nat_int.expr_arith_minus_nat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_minus_nat_nat', async () => {
      await expr_arith_minus_nat_nat.expr_arith_minus_nat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_minus_nat_nat.expr_arith_minus_nat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_minus_nat_nat.expr_arith_minus_nat_nat.exec({ as: alice })
      //      const res_after = await expr_arith_minus_nat_nat.expr_arith_minus_nat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_minus_nat_rat', async () => {
      await expr_arith_minus_nat_rat.expr_arith_minus_nat_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_minus_nat_rat.expr_arith_minus_nat_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_minus_nat_rat.expr_arith_minus_nat_rat.exec({ as: alice })
      //      const res_after = await expr_arith_minus_nat_rat.expr_arith_minus_nat_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_minus_rat_int', async () => {
      await expr_arith_minus_rat_int.expr_arith_minus_rat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_minus_rat_int.expr_arith_minus_rat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_minus_rat_int.expr_arith_minus_rat_int.exec({ as: alice })
      //      const res_after = await expr_arith_minus_rat_int.expr_arith_minus_rat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_minus_rat_nat', async () => {
      await expr_arith_minus_rat_nat.expr_arith_minus_rat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_minus_rat_nat.expr_arith_minus_rat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_minus_rat_nat.expr_arith_minus_rat_nat.exec({ as: alice })
      //      const res_after = await expr_arith_minus_rat_nat.expr_arith_minus_rat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_minus_rat_rat', async () => {
      await expr_arith_minus_rat_rat.expr_arith_minus_rat_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_minus_rat_rat.expr_arith_minus_rat_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_minus_rat_rat.expr_arith_minus_rat_rat.exec({ as: alice })
      //      const res_after = await expr_arith_minus_rat_rat.expr_arith_minus_rat_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_minus_tez_tez', async () => {
      await expr_arith_minus_tez_tez.expr_arith_minus_tez_tez.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_minus_tez_tez.expr_arith_minus_tez_tez.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_minus_tez_tez.expr_arith_minus_tez_tez.exec({ as: alice })
      //      const res_after = await expr_arith_minus_tez_tez.expr_arith_minus_tez_tez.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_mod_int_int', async () => {
      await expr_arith_mod_int_int.expr_arith_mod_int_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_mod_int_int.expr_arith_mod_int_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_mod_int_int.expr_arith_mod_int_int.exec({ as: alice })
      //      const res_after = await expr_arith_mod_int_int.expr_arith_mod_int_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_mod_int_nat', async () => {
      await expr_arith_mod_int_nat.expr_arith_mod_int_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_mod_int_nat.expr_arith_mod_int_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_mod_int_nat.expr_arith_mod_int_nat.exec({ as: alice })
      //      const res_after = await expr_arith_mod_int_nat.expr_arith_mod_int_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_mod_nat_int', async () => {
      await expr_arith_mod_nat_int.expr_arith_mod_nat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_mod_nat_int.expr_arith_mod_nat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_mod_nat_int.expr_arith_mod_nat_int.exec({ as: alice })
      //      const res_after = await expr_arith_mod_nat_int.expr_arith_mod_nat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_mod_nat_nat', async () => {
      await expr_arith_mod_nat_nat.expr_arith_mod_nat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_mod_nat_nat.expr_arith_mod_nat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_mod_nat_nat.expr_arith_mod_nat_nat.exec({ as: alice })
      //      const res_after = await expr_arith_mod_nat_nat.expr_arith_mod_nat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_mod_tez_tez', async () => {
      await expr_arith_mod_tez_tez.expr_arith_mod_tez_tez.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_mod_tez_tez.expr_arith_mod_tez_tez.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_mod_tez_tez.expr_arith_mod_tez_tez.exec({ as: alice })
      //      const res_after = await expr_arith_mod_tez_tez.expr_arith_mod_tez_tez.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_mult_int_dur', async () => {
      await expr_arith_mult_int_dur.expr_arith_mult_int_dur.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_mult_int_dur.expr_arith_mult_int_dur.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_mult_int_dur.expr_arith_mult_int_dur.exec({ as: alice })
      //      const res_after = await expr_arith_mult_int_dur.expr_arith_mult_int_dur.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_mult_int_int', async () => {
      await expr_arith_mult_int_int.expr_arith_mult_int_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_mult_int_int.expr_arith_mult_int_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_mult_int_int.expr_arith_mult_int_int.exec({ as: alice })
      //      const res_after = await expr_arith_mult_int_int.expr_arith_mult_int_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_mult_int_nat', async () => {
      await expr_arith_mult_int_nat.expr_arith_mult_int_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_mult_int_nat.expr_arith_mult_int_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_mult_int_nat.expr_arith_mult_int_nat.exec({ as: alice })
      //      const res_after = await expr_arith_mult_int_nat.expr_arith_mult_int_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_mult_int_rat', async () => {
      await expr_arith_mult_int_rat.expr_arith_mult_int_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_mult_int_rat.expr_arith_mult_int_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_mult_int_rat.expr_arith_mult_int_rat.exec({ as: alice })
      //      const res_after = await expr_arith_mult_int_rat.expr_arith_mult_int_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_mult_int_tez', async () => {
      await expr_arith_mult_int_tez.expr_arith_mult_int_tez.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_mult_int_tez.expr_arith_mult_int_tez.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_mult_int_tez.expr_arith_mult_int_tez.exec({ as: alice })
      //      const res_after = await expr_arith_mult_int_tez.expr_arith_mult_int_tez.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_mult_nat_dur', async () => {
      await expr_arith_mult_nat_dur.expr_arith_mult_nat_dur.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_mult_nat_dur.expr_arith_mult_nat_dur.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_mult_nat_dur.expr_arith_mult_nat_dur.exec({ as: alice })
      //      const res_after = await expr_arith_mult_nat_dur.expr_arith_mult_nat_dur.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_mult_nat_int', async () => {
      await expr_arith_mult_nat_int.expr_arith_mult_nat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_mult_nat_int.expr_arith_mult_nat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_mult_nat_int.expr_arith_mult_nat_int.exec({ as: alice })
      //      const res_after = await expr_arith_mult_nat_int.expr_arith_mult_nat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_mult_nat_nat', async () => {
      await expr_arith_mult_nat_nat.expr_arith_mult_nat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_mult_nat_nat.expr_arith_mult_nat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_mult_nat_nat.expr_arith_mult_nat_nat.exec({ as: alice })
      //      const res_after = await expr_arith_mult_nat_nat.expr_arith_mult_nat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_mult_nat_rat', async () => {
      await expr_arith_mult_nat_rat.expr_arith_mult_nat_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_mult_nat_rat.expr_arith_mult_nat_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_mult_nat_rat.expr_arith_mult_nat_rat.exec({ as: alice })
      //      const res_after = await expr_arith_mult_nat_rat.expr_arith_mult_nat_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_mult_nat_tez', async () => {
      await expr_arith_mult_nat_tez.expr_arith_mult_nat_tez.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_mult_nat_tez.expr_arith_mult_nat_tez.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_mult_nat_tez.expr_arith_mult_nat_tez.exec({ as: alice })
      //      const res_after = await expr_arith_mult_nat_tez.expr_arith_mult_nat_tez.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_mult_rat_dur', async () => {
      await expr_arith_mult_rat_dur.expr_arith_mult_rat_dur.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_mult_rat_dur.expr_arith_mult_rat_dur.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_mult_rat_dur.expr_arith_mult_rat_dur.exec({ as: alice })
      //      const res_after = await expr_arith_mult_rat_dur.expr_arith_mult_rat_dur.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_mult_rat_int', async () => {
      await expr_arith_mult_rat_int.expr_arith_mult_rat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_mult_rat_int.expr_arith_mult_rat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_mult_rat_int.expr_arith_mult_rat_int.exec({ as: alice })
      //      const res_after = await expr_arith_mult_rat_int.expr_arith_mult_rat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_mult_rat_nat', async () => {
      await expr_arith_mult_rat_nat.expr_arith_mult_rat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_mult_rat_nat.expr_arith_mult_rat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_mult_rat_nat.expr_arith_mult_rat_nat.exec({ as: alice })
      //      const res_after = await expr_arith_mult_rat_nat.expr_arith_mult_rat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_mult_rat_rat', async () => {
      await expr_arith_mult_rat_rat.expr_arith_mult_rat_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_mult_rat_rat.expr_arith_mult_rat_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_mult_rat_rat.expr_arith_mult_rat_rat.exec({ as: alice })
      //      const res_after = await expr_arith_mult_rat_rat.expr_arith_mult_rat_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_mult_rat_tez', async () => {
      await expr_arith_mult_rat_tez.expr_arith_mult_rat_tez.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_mult_rat_tez.expr_arith_mult_rat_tez.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_mult_rat_tez.expr_arith_mult_rat_tez.exec({ as: alice })
      //      const res_after = await expr_arith_mult_rat_tez.expr_arith_mult_rat_tez.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_mult_tez_nat', async () => {
      await expr_arith_mult_tez_nat.expr_arith_mult_tez_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_mult_tez_nat.expr_arith_mult_tez_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_mult_tez_nat.expr_arith_mult_tez_nat.exec({ as: alice })
      //      const res_after = await expr_arith_mult_tez_nat.expr_arith_mult_tez_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_not_bool', async () => {
      await expr_arith_not_bool.expr_arith_not_bool.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_not_bool.expr_arith_not_bool.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_not_bool.expr_arith_not_bool.exec({ as: alice })
      //      const res_after = await expr_arith_not_bool.expr_arith_not_bool.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_not_int', async () => {
      await expr_arith_not_int.expr_arith_not_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_not_int.expr_arith_not_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_not_int.expr_arith_not_int.exec({ as: alice })
      //      const res_after = await expr_arith_not_int.expr_arith_not_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_not_nat', async () => {
      await expr_arith_not_nat.expr_arith_not_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_not_nat.expr_arith_not_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_not_nat.expr_arith_not_nat.exec({ as: alice })
      //      const res_after = await expr_arith_not_nat.expr_arith_not_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_or_bool_bool', async () => {
      await expr_arith_or_bool_bool.expr_arith_or_bool_bool.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_or_bool_bool.expr_arith_or_bool_bool.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_or_bool_bool.expr_arith_or_bool_bool.exec({ as: alice })
      //      const res_after = await expr_arith_or_bool_bool.expr_arith_or_bool_bool.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_or_nat_nat', async () => {
      await expr_arith_or_nat_nat.expr_arith_or_nat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_or_nat_nat.expr_arith_or_nat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_or_nat_nat.expr_arith_or_nat_nat.exec({ as: alice })
      //      const res_after = await expr_arith_or_nat_nat.expr_arith_or_nat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_plus_date_dur', async () => {
      await expr_arith_plus_date_dur.expr_arith_plus_date_dur.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_plus_date_dur.expr_arith_plus_date_dur.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_plus_date_dur.expr_arith_plus_date_dur.exec({ as: alice })
      //      const res_after = await expr_arith_plus_date_dur.expr_arith_plus_date_dur.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_plus_dur_date', async () => {
      await expr_arith_plus_dur_date.expr_arith_plus_dur_date.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_plus_dur_date.expr_arith_plus_dur_date.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_plus_dur_date.expr_arith_plus_dur_date.exec({ as: alice })
      //      const res_after = await expr_arith_plus_dur_date.expr_arith_plus_dur_date.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_plus_dur_dur', async () => {
      await expr_arith_plus_dur_dur.expr_arith_plus_dur_dur.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_plus_dur_dur.expr_arith_plus_dur_dur.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_plus_dur_dur.expr_arith_plus_dur_dur.exec({ as: alice })
      //      const res_after = await expr_arith_plus_dur_dur.expr_arith_plus_dur_dur.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_plus_int_int', async () => {
      await expr_arith_plus_int_int.expr_arith_plus_int_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_plus_int_int.expr_arith_plus_int_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_plus_int_int.expr_arith_plus_int_int.exec({ as: alice })
      //      const res_after = await expr_arith_plus_int_int.expr_arith_plus_int_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_plus_int_nat', async () => {
      await expr_arith_plus_int_nat.expr_arith_plus_int_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_plus_int_nat.expr_arith_plus_int_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_plus_int_nat.expr_arith_plus_int_nat.exec({ as: alice })
      //      const res_after = await expr_arith_plus_int_nat.expr_arith_plus_int_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_plus_int_rat', async () => {
      await expr_arith_plus_int_rat.expr_arith_plus_int_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_plus_int_rat.expr_arith_plus_int_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_plus_int_rat.expr_arith_plus_int_rat.exec({ as: alice })
      //      const res_after = await expr_arith_plus_int_rat.expr_arith_plus_int_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_plus_nat_int', async () => {
      await expr_arith_plus_nat_int.expr_arith_plus_nat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_plus_nat_int.expr_arith_plus_nat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_plus_nat_int.expr_arith_plus_nat_int.exec({ as: alice })
      //      const res_after = await expr_arith_plus_nat_int.expr_arith_plus_nat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_plus_nat_nat', async () => {
      await expr_arith_plus_nat_nat.expr_arith_plus_nat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_plus_nat_nat.expr_arith_plus_nat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_plus_nat_nat.expr_arith_plus_nat_nat.exec({ as: alice })
      //      const res_after = await expr_arith_plus_nat_nat.expr_arith_plus_nat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_plus_nat_rat', async () => {
      await expr_arith_plus_nat_rat.expr_arith_plus_nat_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_plus_nat_rat.expr_arith_plus_nat_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_plus_nat_rat.expr_arith_plus_nat_rat.exec({ as: alice })
      //      const res_after = await expr_arith_plus_nat_rat.expr_arith_plus_nat_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_plus_rat_int', async () => {
      await expr_arith_plus_rat_int.expr_arith_plus_rat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_plus_rat_int.expr_arith_plus_rat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_plus_rat_int.expr_arith_plus_rat_int.exec({ as: alice })
      //      const res_after = await expr_arith_plus_rat_int.expr_arith_plus_rat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_plus_rat_nat', async () => {
      await expr_arith_plus_rat_nat.expr_arith_plus_rat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_plus_rat_nat.expr_arith_plus_rat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_plus_rat_nat.expr_arith_plus_rat_nat.exec({ as: alice })
      //      const res_after = await expr_arith_plus_rat_nat.expr_arith_plus_rat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_plus_rat_rat', async () => {
      await expr_arith_plus_rat_rat.expr_arith_plus_rat_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_plus_rat_rat.expr_arith_plus_rat_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_plus_rat_rat.expr_arith_plus_rat_rat.exec({ as: alice })
      //      const res_after = await expr_arith_plus_rat_rat.expr_arith_plus_rat_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_plus_str_str', async () => {
      await expr_arith_plus_str_str.expr_arith_plus_str_str.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_plus_str_str.expr_arith_plus_str_str.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_plus_str_str.expr_arith_plus_str_str.exec({ as: alice })
      //      const res_after = await expr_arith_plus_str_str.expr_arith_plus_str_str.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_plus_tez_tez', async () => {
      await expr_arith_plus_tez_tez.expr_arith_plus_tez_tez.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_plus_tez_tez.expr_arith_plus_tez_tez.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_plus_tez_tez.expr_arith_plus_tez_tez.exec({ as: alice })
      //      const res_after = await expr_arith_plus_tez_tez.expr_arith_plus_tez_tez.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_uminus_int', async () => {
      await expr_arith_uminus_int.expr_arith_uminus_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_uminus_int.expr_arith_uminus_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_uminus_int.expr_arith_uminus_int.exec({ as: alice })
      //      const res_after = await expr_arith_uminus_int.expr_arith_uminus_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_uminus_rat', async () => {
      await expr_arith_uminus_rat.expr_arith_uminus_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_uminus_rat.expr_arith_uminus_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_uminus_rat.expr_arith_uminus_rat.exec({ as: alice })
      //      const res_after = await expr_arith_uminus_rat.expr_arith_uminus_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_xor_bool_bool', async () => {
      await expr_arith_xor_bool_bool.expr_arith_xor_bool_bool.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_xor_bool_bool.expr_arith_xor_bool_bool.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_xor_bool_bool.expr_arith_xor_bool_bool.exec({ as: alice })
      //      const res_after = await expr_arith_xor_bool_bool.expr_arith_xor_bool_bool.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_arith_xor_nat_nat', async () => {
      await expr_arith_xor_nat_nat.expr_arith_xor_nat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_arith_xor_nat_nat.expr_arith_xor_nat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_arith_xor_nat_nat.expr_arith_xor_nat_nat.exec({ as: alice })
      //      const res_after = await expr_arith_xor_nat_nat.expr_arith_xor_nat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    it('expr_cmp_eq_addr_addr', async () => {
      await expr_cmp_eq_addr_addr.expr_cmp_eq_addr_addr.deploy({ as: alice })
      const before_expected = false
      const after_expected = true
      const res_before = await expr_cmp_eq_addr_addr.expr_cmp_eq_addr_addr.get_res();
      assert(res_before == before_expected, "Invalid Value")
      await expr_cmp_eq_addr_addr.expr_cmp_eq_addr_addr.exec({ as: alice })
      const res_after = await expr_cmp_eq_addr_addr.expr_cmp_eq_addr_addr.get_res();
      assert(after_expected == after_expected, "Invalid Value")
    })

    // TODO
    it('expr_cmp_eq_bool_bool', async () => {
      await expr_cmp_eq_bool_bool.expr_cmp_eq_bool_bool.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_eq_bool_bool.expr_cmp_eq_bool_bool.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_eq_bool_bool.expr_cmp_eq_bool_bool.exec({ as: alice })
      //      const res_after = await expr_cmp_eq_bool_bool.expr_cmp_eq_bool_bool.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_eq_date_date', async () => {
      await expr_cmp_eq_date_date.expr_cmp_eq_date_date.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_eq_date_date.expr_cmp_eq_date_date.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_eq_date_date.expr_cmp_eq_date_date.exec({ as: alice })
      //      const res_after = await expr_cmp_eq_date_date.expr_cmp_eq_date_date.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_eq_dur_dur', async () => {
      await expr_cmp_eq_dur_dur.expr_cmp_eq_dur_dur.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_eq_dur_dur.expr_cmp_eq_dur_dur.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_eq_dur_dur.expr_cmp_eq_dur_dur.exec({ as: alice })
      //      const res_after = await expr_cmp_eq_dur_dur.expr_cmp_eq_dur_dur.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_eq_int_int', async () => {
      await expr_cmp_eq_int_int.expr_cmp_eq_int_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_eq_int_int.expr_cmp_eq_int_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_eq_int_int.expr_cmp_eq_int_int.exec({ as: alice })
      //      const res_after = await expr_cmp_eq_int_int.expr_cmp_eq_int_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_eq_int_nat', async () => {
      await expr_cmp_eq_int_nat.expr_cmp_eq_int_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_eq_int_nat.expr_cmp_eq_int_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_eq_int_nat.expr_cmp_eq_int_nat.exec({ as: alice })
      //      const res_after = await expr_cmp_eq_int_nat.expr_cmp_eq_int_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_eq_int_rat', async () => {
      await expr_cmp_eq_int_rat.expr_cmp_eq_int_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_eq_int_rat.expr_cmp_eq_int_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_eq_int_rat.expr_cmp_eq_int_rat.exec({ as: alice })
      //      const res_after = await expr_cmp_eq_int_rat.expr_cmp_eq_int_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_eq_nat_int', async () => {
      await expr_cmp_eq_nat_int.expr_cmp_eq_nat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_eq_nat_int.expr_cmp_eq_nat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_eq_nat_int.expr_cmp_eq_nat_int.exec({ as: alice })
      //      const res_after = await expr_cmp_eq_nat_int.expr_cmp_eq_nat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_eq_nat_nat', async () => {
      await expr_cmp_eq_nat_nat.expr_cmp_eq_nat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_eq_nat_nat.expr_cmp_eq_nat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_eq_nat_nat.expr_cmp_eq_nat_nat.exec({ as: alice })
      //      const res_after = await expr_cmp_eq_nat_nat.expr_cmp_eq_nat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_eq_nat_rat', async () => {
      await expr_cmp_eq_nat_rat.expr_cmp_eq_nat_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_eq_nat_rat.expr_cmp_eq_nat_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_eq_nat_rat.expr_cmp_eq_nat_rat.exec({ as: alice })
      //      const res_after = await expr_cmp_eq_nat_rat.expr_cmp_eq_nat_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_eq_rat_int', async () => {
      await expr_cmp_eq_rat_int.expr_cmp_eq_rat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_eq_rat_int.expr_cmp_eq_rat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_eq_rat_int.expr_cmp_eq_rat_int.exec({ as: alice })
      //      const res_after = await expr_cmp_eq_rat_int.expr_cmp_eq_rat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_eq_rat_nat', async () => {
      await expr_cmp_eq_rat_nat.expr_cmp_eq_rat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_eq_rat_nat.expr_cmp_eq_rat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_eq_rat_nat.expr_cmp_eq_rat_nat.exec({ as: alice })
      //      const res_after = await expr_cmp_eq_rat_nat.expr_cmp_eq_rat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_eq_rat_rat', async () => {
      await expr_cmp_eq_rat_rat.expr_cmp_eq_rat_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_eq_rat_rat.expr_cmp_eq_rat_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_eq_rat_rat.expr_cmp_eq_rat_rat.exec({ as: alice })
      //      const res_after = await expr_cmp_eq_rat_rat.expr_cmp_eq_rat_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_eq_str_str', async () => {
      await expr_cmp_eq_str_str.expr_cmp_eq_str_str.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_eq_str_str.expr_cmp_eq_str_str.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_eq_str_str.expr_cmp_eq_str_str.exec({ as: alice })
      //      const res_after = await expr_cmp_eq_str_str.expr_cmp_eq_str_str.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_eq_tez_tez', async () => {
      await expr_cmp_eq_tez_tez.expr_cmp_eq_tez_tez.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_eq_tez_tez.expr_cmp_eq_tez_tez.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_eq_tez_tez.expr_cmp_eq_tez_tez.exec({ as: alice })
      //      const res_after = await expr_cmp_eq_tez_tez.expr_cmp_eq_tez_tez.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_ge_addr_addr', async () => {
      await expr_cmp_ge_addr_addr.expr_cmp_ge_addr_addr.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_ge_addr_addr.expr_cmp_ge_addr_addr.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_ge_addr_addr.expr_cmp_ge_addr_addr.exec({ as: alice })
      //      const res_after = await expr_cmp_ge_addr_addr.expr_cmp_ge_addr_addr.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_ge_date_date', async () => {
      await expr_cmp_ge_date_date.expr_cmp_ge_date_date.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_ge_date_date.expr_cmp_ge_date_date.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_ge_date_date.expr_cmp_ge_date_date.exec({ as: alice })
      //      const res_after = await expr_cmp_ge_date_date.expr_cmp_ge_date_date.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_ge_dur_dur', async () => {
      await expr_cmp_ge_dur_dur.expr_cmp_ge_dur_dur.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_ge_dur_dur.expr_cmp_ge_dur_dur.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_ge_dur_dur.expr_cmp_ge_dur_dur.exec({ as: alice })
      //      const res_after = await expr_cmp_ge_dur_dur.expr_cmp_ge_dur_dur.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_ge_int_int', async () => {
      await expr_cmp_ge_int_int.expr_cmp_ge_int_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_ge_int_int.expr_cmp_ge_int_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_ge_int_int.expr_cmp_ge_int_int.exec({ as: alice })
      //      const res_after = await expr_cmp_ge_int_int.expr_cmp_ge_int_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_ge_int_nat', async () => {
      await expr_cmp_ge_int_nat.expr_cmp_ge_int_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_ge_int_nat.expr_cmp_ge_int_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_ge_int_nat.expr_cmp_ge_int_nat.exec({ as: alice })
      //      const res_after = await expr_cmp_ge_int_nat.expr_cmp_ge_int_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_ge_int_rat', async () => {
      await expr_cmp_ge_int_rat.expr_cmp_ge_int_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_ge_int_rat.expr_cmp_ge_int_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_ge_int_rat.expr_cmp_ge_int_rat.exec({ as: alice })
      //      const res_after = await expr_cmp_ge_int_rat.expr_cmp_ge_int_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_ge_nat_int', async () => {
      await expr_cmp_ge_nat_int.expr_cmp_ge_nat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_ge_nat_int.expr_cmp_ge_nat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_ge_nat_int.expr_cmp_ge_nat_int.exec({ as: alice })
      //      const res_after = await expr_cmp_ge_nat_int.expr_cmp_ge_nat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_ge_nat_nat', async () => {
      await expr_cmp_ge_nat_nat.expr_cmp_ge_nat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_ge_nat_nat.expr_cmp_ge_nat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_ge_nat_nat.expr_cmp_ge_nat_nat.exec({ as: alice })
      //      const res_after = await expr_cmp_ge_nat_nat.expr_cmp_ge_nat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_ge_nat_rat', async () => {
      await expr_cmp_ge_nat_rat.expr_cmp_ge_nat_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_ge_nat_rat.expr_cmp_ge_nat_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_ge_nat_rat.expr_cmp_ge_nat_rat.exec({ as: alice })
      //      const res_after = await expr_cmp_ge_nat_rat.expr_cmp_ge_nat_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_ge_rat_int', async () => {
      await expr_cmp_ge_rat_int.expr_cmp_ge_rat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_ge_rat_int.expr_cmp_ge_rat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_ge_rat_int.expr_cmp_ge_rat_int.exec({ as: alice })
      //      const res_after = await expr_cmp_ge_rat_int.expr_cmp_ge_rat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_ge_rat_nat', async () => {
      await expr_cmp_ge_rat_nat.expr_cmp_ge_rat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_ge_rat_nat.expr_cmp_ge_rat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_ge_rat_nat.expr_cmp_ge_rat_nat.exec({ as: alice })
      //      const res_after = await expr_cmp_ge_rat_nat.expr_cmp_ge_rat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_ge_rat_rat', async () => {
      await expr_cmp_ge_rat_rat.expr_cmp_ge_rat_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_ge_rat_rat.expr_cmp_ge_rat_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_ge_rat_rat.expr_cmp_ge_rat_rat.exec({ as: alice })
      //      const res_after = await expr_cmp_ge_rat_rat.expr_cmp_ge_rat_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_ge_str_str', async () => {
      await expr_cmp_ge_str_str.expr_cmp_ge_str_str.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_ge_str_str.expr_cmp_ge_str_str.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_ge_str_str.expr_cmp_ge_str_str.exec({ as: alice })
      //      const res_after = await expr_cmp_ge_str_str.expr_cmp_ge_str_str.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_ge_tez_tez', async () => {
      await expr_cmp_ge_tez_tez.expr_cmp_ge_tez_tez.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_ge_tez_tez.expr_cmp_ge_tez_tez.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_ge_tez_tez.expr_cmp_ge_tez_tez.exec({ as: alice })
      //      const res_after = await expr_cmp_ge_tez_tez.expr_cmp_ge_tez_tez.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_gt_addr_addr', async () => {
      await expr_cmp_gt_addr_addr.expr_cmp_gt_addr_addr.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_gt_addr_addr.expr_cmp_gt_addr_addr.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_gt_addr_addr.expr_cmp_gt_addr_addr.exec({ as: alice })
      //      const res_after = await expr_cmp_gt_addr_addr.expr_cmp_gt_addr_addr.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_gt_date_date', async () => {
      await expr_cmp_gt_date_date.expr_cmp_gt_date_date.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_gt_date_date.expr_cmp_gt_date_date.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_gt_date_date.expr_cmp_gt_date_date.exec({ as: alice })
      //      const res_after = await expr_cmp_gt_date_date.expr_cmp_gt_date_date.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_gt_dur_dur', async () => {
      await expr_cmp_gt_dur_dur.expr_cmp_gt_dur_dur.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_gt_dur_dur.expr_cmp_gt_dur_dur.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_gt_dur_dur.expr_cmp_gt_dur_dur.exec({ as: alice })
      //      const res_after = await expr_cmp_gt_dur_dur.expr_cmp_gt_dur_dur.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_gt_int_int', async () => {
      await expr_cmp_gt_int_int.expr_cmp_gt_int_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_gt_int_int.expr_cmp_gt_int_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_gt_int_int.expr_cmp_gt_int_int.exec({ as: alice })
      //      const res_after = await expr_cmp_gt_int_int.expr_cmp_gt_int_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_gt_int_nat', async () => {
      await expr_cmp_gt_int_nat.expr_cmp_gt_int_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_gt_int_nat.expr_cmp_gt_int_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_gt_int_nat.expr_cmp_gt_int_nat.exec({ as: alice })
      //      const res_after = await expr_cmp_gt_int_nat.expr_cmp_gt_int_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_gt_int_rat', async () => {
      await expr_cmp_gt_int_rat.expr_cmp_gt_int_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_gt_int_rat.expr_cmp_gt_int_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_gt_int_rat.expr_cmp_gt_int_rat.exec({ as: alice })
      //      const res_after = await expr_cmp_gt_int_rat.expr_cmp_gt_int_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_gt_nat_int', async () => {
      await expr_cmp_gt_nat_int.expr_cmp_gt_nat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_gt_nat_int.expr_cmp_gt_nat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_gt_nat_int.expr_cmp_gt_nat_int.exec({ as: alice })
      //      const res_after = await expr_cmp_gt_nat_int.expr_cmp_gt_nat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_gt_nat_nat', async () => {
      await expr_cmp_gt_nat_nat.expr_cmp_gt_nat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_gt_nat_nat.expr_cmp_gt_nat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_gt_nat_nat.expr_cmp_gt_nat_nat.exec({ as: alice })
      //      const res_after = await expr_cmp_gt_nat_nat.expr_cmp_gt_nat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_gt_nat_rat', async () => {
      await expr_cmp_gt_nat_rat.expr_cmp_gt_nat_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_gt_nat_rat.expr_cmp_gt_nat_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_gt_nat_rat.expr_cmp_gt_nat_rat.exec({ as: alice })
      //      const res_after = await expr_cmp_gt_nat_rat.expr_cmp_gt_nat_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_gt_rat_int', async () => {
      await expr_cmp_gt_rat_int.expr_cmp_gt_rat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_gt_rat_int.expr_cmp_gt_rat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_gt_rat_int.expr_cmp_gt_rat_int.exec({ as: alice })
      //      const res_after = await expr_cmp_gt_rat_int.expr_cmp_gt_rat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_gt_rat_nat', async () => {
      await expr_cmp_gt_rat_nat.expr_cmp_gt_rat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_gt_rat_nat.expr_cmp_gt_rat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_gt_rat_nat.expr_cmp_gt_rat_nat.exec({ as: alice })
      //      const res_after = await expr_cmp_gt_rat_nat.expr_cmp_gt_rat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_gt_rat_rat', async () => {
      await expr_cmp_gt_rat_rat.expr_cmp_gt_rat_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_gt_rat_rat.expr_cmp_gt_rat_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_gt_rat_rat.expr_cmp_gt_rat_rat.exec({ as: alice })
      //      const res_after = await expr_cmp_gt_rat_rat.expr_cmp_gt_rat_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_gt_str_str', async () => {
      await expr_cmp_gt_str_str.expr_cmp_gt_str_str.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_gt_str_str.expr_cmp_gt_str_str.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_gt_str_str.expr_cmp_gt_str_str.exec({ as: alice })
      //      const res_after = await expr_cmp_gt_str_str.expr_cmp_gt_str_str.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_gt_tez_tez', async () => {
      await expr_cmp_gt_tez_tez.expr_cmp_gt_tez_tez.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_gt_tez_tez.expr_cmp_gt_tez_tez.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_gt_tez_tez.expr_cmp_gt_tez_tez.exec({ as: alice })
      //      const res_after = await expr_cmp_gt_tez_tez.expr_cmp_gt_tez_tez.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_le_addr_addr', async () => {
      await expr_cmp_le_addr_addr.expr_cmp_le_addr_addr.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_le_addr_addr.expr_cmp_le_addr_addr.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_le_addr_addr.expr_cmp_le_addr_addr.exec({ as: alice })
      //      const res_after = await expr_cmp_le_addr_addr.expr_cmp_le_addr_addr.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_le_date_date', async () => {
      await expr_cmp_le_date_date.expr_cmp_le_date_date.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_le_date_date.expr_cmp_le_date_date.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_le_date_date.expr_cmp_le_date_date.exec({ as: alice })
      //      const res_after = await expr_cmp_le_date_date.expr_cmp_le_date_date.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_le_dur_dur', async () => {
      await expr_cmp_le_dur_dur.expr_cmp_le_dur_dur.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_le_dur_dur.expr_cmp_le_dur_dur.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_le_dur_dur.expr_cmp_le_dur_dur.exec({ as: alice })
      //      const res_after = await expr_cmp_le_dur_dur.expr_cmp_le_dur_dur.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_le_int_int', async () => {
      await expr_cmp_le_int_int.expr_cmp_le_int_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_le_int_int.expr_cmp_le_int_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_le_int_int.expr_cmp_le_int_int.exec({ as: alice })
      //      const res_after = await expr_cmp_le_int_int.expr_cmp_le_int_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_le_int_nat', async () => {
      await expr_cmp_le_int_nat.expr_cmp_le_int_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_le_int_nat.expr_cmp_le_int_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_le_int_nat.expr_cmp_le_int_nat.exec({ as: alice })
      //      const res_after = await expr_cmp_le_int_nat.expr_cmp_le_int_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_le_int_rat', async () => {
      await expr_cmp_le_int_rat.expr_cmp_le_int_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_le_int_rat.expr_cmp_le_int_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_le_int_rat.expr_cmp_le_int_rat.exec({ as: alice })
      //      const res_after = await expr_cmp_le_int_rat.expr_cmp_le_int_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_le_nat_int', async () => {
      await expr_cmp_le_nat_int.expr_cmp_le_nat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_le_nat_int.expr_cmp_le_nat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_le_nat_int.expr_cmp_le_nat_int.exec({ as: alice })
      //      const res_after = await expr_cmp_le_nat_int.expr_cmp_le_nat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_le_nat_nat', async () => {
      await expr_cmp_le_nat_nat.expr_cmp_le_nat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_le_nat_nat.expr_cmp_le_nat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_le_nat_nat.expr_cmp_le_nat_nat.exec({ as: alice })
      //      const res_after = await expr_cmp_le_nat_nat.expr_cmp_le_nat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_le_nat_rat', async () => {
      await expr_cmp_le_nat_rat.expr_cmp_le_nat_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_le_nat_rat.expr_cmp_le_nat_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_le_nat_rat.expr_cmp_le_nat_rat.exec({ as: alice })
      //      const res_after = await expr_cmp_le_nat_rat.expr_cmp_le_nat_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_le_rat_int', async () => {
      await expr_cmp_le_rat_int.expr_cmp_le_rat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_le_rat_int.expr_cmp_le_rat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_le_rat_int.expr_cmp_le_rat_int.exec({ as: alice })
      //      const res_after = await expr_cmp_le_rat_int.expr_cmp_le_rat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_le_rat_nat', async () => {
      await expr_cmp_le_rat_nat.expr_cmp_le_rat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_le_rat_nat.expr_cmp_le_rat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_le_rat_nat.expr_cmp_le_rat_nat.exec({ as: alice })
      //      const res_after = await expr_cmp_le_rat_nat.expr_cmp_le_rat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_le_rat_rat', async () => {
      await expr_cmp_le_rat_rat.expr_cmp_le_rat_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_le_rat_rat.expr_cmp_le_rat_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_le_rat_rat.expr_cmp_le_rat_rat.exec({ as: alice })
      //      const res_after = await expr_cmp_le_rat_rat.expr_cmp_le_rat_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_le_str_str', async () => {
      await expr_cmp_le_str_str.expr_cmp_le_str_str.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_le_str_str.expr_cmp_le_str_str.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_le_str_str.expr_cmp_le_str_str.exec({ as: alice })
      //      const res_after = await expr_cmp_le_str_str.expr_cmp_le_str_str.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_le_tez_tez', async () => {
      await expr_cmp_le_tez_tez.expr_cmp_le_tez_tez.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_le_tez_tez.expr_cmp_le_tez_tez.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_le_tez_tez.expr_cmp_le_tez_tez.exec({ as: alice })
      //      const res_after = await expr_cmp_le_tez_tez.expr_cmp_le_tez_tez.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_lt_addr_addr', async () => {
      await expr_cmp_lt_addr_addr.expr_cmp_lt_addr_addr.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_lt_addr_addr.expr_cmp_lt_addr_addr.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_lt_addr_addr.expr_cmp_lt_addr_addr.exec({ as: alice })
      //      const res_after = await expr_cmp_lt_addr_addr.expr_cmp_lt_addr_addr.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_lt_date_date', async () => {
      await expr_cmp_lt_date_date.expr_cmp_lt_date_date.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_lt_date_date.expr_cmp_lt_date_date.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_lt_date_date.expr_cmp_lt_date_date.exec({ as: alice })
      //      const res_after = await expr_cmp_lt_date_date.expr_cmp_lt_date_date.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_lt_dur_dur', async () => {
      await expr_cmp_lt_dur_dur.expr_cmp_lt_dur_dur.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_lt_dur_dur.expr_cmp_lt_dur_dur.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_lt_dur_dur.expr_cmp_lt_dur_dur.exec({ as: alice })
      //      const res_after = await expr_cmp_lt_dur_dur.expr_cmp_lt_dur_dur.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_lt_int_int', async () => {
      await expr_cmp_lt_int_int.expr_cmp_lt_int_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_lt_int_int.expr_cmp_lt_int_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_lt_int_int.expr_cmp_lt_int_int.exec({ as: alice })
      //      const res_after = await expr_cmp_lt_int_int.expr_cmp_lt_int_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_lt_int_nat', async () => {
      await expr_cmp_lt_int_nat.expr_cmp_lt_int_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_lt_int_nat.expr_cmp_lt_int_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_lt_int_nat.expr_cmp_lt_int_nat.exec({ as: alice })
      //      const res_after = await expr_cmp_lt_int_nat.expr_cmp_lt_int_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_lt_int_rat', async () => {
      await expr_cmp_lt_int_rat.expr_cmp_lt_int_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_lt_int_rat.expr_cmp_lt_int_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_lt_int_rat.expr_cmp_lt_int_rat.exec({ as: alice })
      //      const res_after = await expr_cmp_lt_int_rat.expr_cmp_lt_int_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_lt_nat_int', async () => {
      await expr_cmp_lt_nat_int.expr_cmp_lt_nat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_lt_nat_int.expr_cmp_lt_nat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_lt_nat_int.expr_cmp_lt_nat_int.exec({ as: alice })
      //      const res_after = await expr_cmp_lt_nat_int.expr_cmp_lt_nat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_lt_nat_nat', async () => {
      await expr_cmp_lt_nat_nat.expr_cmp_lt_nat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_lt_nat_nat.expr_cmp_lt_nat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_lt_nat_nat.expr_cmp_lt_nat_nat.exec({ as: alice })
      //      const res_after = await expr_cmp_lt_nat_nat.expr_cmp_lt_nat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_lt_nat_rat', async () => {
      await expr_cmp_lt_nat_rat.expr_cmp_lt_nat_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_lt_nat_rat.expr_cmp_lt_nat_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_lt_nat_rat.expr_cmp_lt_nat_rat.exec({ as: alice })
      //      const res_after = await expr_cmp_lt_nat_rat.expr_cmp_lt_nat_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_lt_rat_int', async () => {
      await expr_cmp_lt_rat_int.expr_cmp_lt_rat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_lt_rat_int.expr_cmp_lt_rat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_lt_rat_int.expr_cmp_lt_rat_int.exec({ as: alice })
      //      const res_after = await expr_cmp_lt_rat_int.expr_cmp_lt_rat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_lt_rat_nat', async () => {
      await expr_cmp_lt_rat_nat.expr_cmp_lt_rat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_lt_rat_nat.expr_cmp_lt_rat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_lt_rat_nat.expr_cmp_lt_rat_nat.exec({ as: alice })
      //      const res_after = await expr_cmp_lt_rat_nat.expr_cmp_lt_rat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_lt_rat_rat', async () => {
      await expr_cmp_lt_rat_rat.expr_cmp_lt_rat_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_lt_rat_rat.expr_cmp_lt_rat_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_lt_rat_rat.expr_cmp_lt_rat_rat.exec({ as: alice })
      //      const res_after = await expr_cmp_lt_rat_rat.expr_cmp_lt_rat_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_lt_str_str', async () => {
      await expr_cmp_lt_str_str.expr_cmp_lt_str_str.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_lt_str_str.expr_cmp_lt_str_str.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_lt_str_str.expr_cmp_lt_str_str.exec({ as: alice })
      //      const res_after = await expr_cmp_lt_str_str.expr_cmp_lt_str_str.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_lt_tez_tez', async () => {
      await expr_cmp_lt_tez_tez.expr_cmp_lt_tez_tez.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_lt_tez_tez.expr_cmp_lt_tez_tez.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_lt_tez_tez.expr_cmp_lt_tez_tez.exec({ as: alice })
      //      const res_after = await expr_cmp_lt_tez_tez.expr_cmp_lt_tez_tez.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_ne_addr_addr', async () => {
      await expr_cmp_ne_addr_addr.expr_cmp_ne_addr_addr.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_ne_addr_addr.expr_cmp_ne_addr_addr.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_ne_addr_addr.expr_cmp_ne_addr_addr.exec({ as: alice })
      //      const res_after = await expr_cmp_ne_addr_addr.expr_cmp_ne_addr_addr.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_ne_bool_bool', async () => {
      await expr_cmp_ne_bool_bool.expr_cmp_ne_bool_bool.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_ne_bool_bool.expr_cmp_ne_bool_bool.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_ne_bool_bool.expr_cmp_ne_bool_bool.exec({ as: alice })
      //      const res_after = await expr_cmp_ne_bool_bool.expr_cmp_ne_bool_bool.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_ne_date_date', async () => {
      await expr_cmp_ne_date_date.expr_cmp_ne_date_date.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_ne_date_date.expr_cmp_ne_date_date.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_ne_date_date.expr_cmp_ne_date_date.exec({ as: alice })
      //      const res_after = await expr_cmp_ne_date_date.expr_cmp_ne_date_date.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_ne_dur_dur', async () => {
      await expr_cmp_ne_dur_dur.expr_cmp_ne_dur_dur.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_ne_dur_dur.expr_cmp_ne_dur_dur.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_ne_dur_dur.expr_cmp_ne_dur_dur.exec({ as: alice })
      //      const res_after = await expr_cmp_ne_dur_dur.expr_cmp_ne_dur_dur.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_ne_int_int', async () => {
      await expr_cmp_ne_int_int.expr_cmp_ne_int_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_ne_int_int.expr_cmp_ne_int_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_ne_int_int.expr_cmp_ne_int_int.exec({ as: alice })
      //      const res_after = await expr_cmp_ne_int_int.expr_cmp_ne_int_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_ne_int_nat', async () => {
      await expr_cmp_ne_int_nat.expr_cmp_ne_int_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_ne_int_nat.expr_cmp_ne_int_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_ne_int_nat.expr_cmp_ne_int_nat.exec({ as: alice })
      //      const res_after = await expr_cmp_ne_int_nat.expr_cmp_ne_int_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_ne_int_rat', async () => {
      await expr_cmp_ne_int_rat.expr_cmp_ne_int_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_ne_int_rat.expr_cmp_ne_int_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_ne_int_rat.expr_cmp_ne_int_rat.exec({ as: alice })
      //      const res_after = await expr_cmp_ne_int_rat.expr_cmp_ne_int_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_ne_nat_int', async () => {
      await expr_cmp_ne_nat_int.expr_cmp_ne_nat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_ne_nat_int.expr_cmp_ne_nat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_ne_nat_int.expr_cmp_ne_nat_int.exec({ as: alice })
      //      const res_after = await expr_cmp_ne_nat_int.expr_cmp_ne_nat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_ne_nat_nat', async () => {
      await expr_cmp_ne_nat_nat.expr_cmp_ne_nat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_ne_nat_nat.expr_cmp_ne_nat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_ne_nat_nat.expr_cmp_ne_nat_nat.exec({ as: alice })
      //      const res_after = await expr_cmp_ne_nat_nat.expr_cmp_ne_nat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_ne_nat_rat', async () => {
      await expr_cmp_ne_nat_rat.expr_cmp_ne_nat_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_ne_nat_rat.expr_cmp_ne_nat_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_ne_nat_rat.expr_cmp_ne_nat_rat.exec({ as: alice })
      //      const res_after = await expr_cmp_ne_nat_rat.expr_cmp_ne_nat_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_ne_rat_int', async () => {
      await expr_cmp_ne_rat_int.expr_cmp_ne_rat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_ne_rat_int.expr_cmp_ne_rat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_ne_rat_int.expr_cmp_ne_rat_int.exec({ as: alice })
      //      const res_after = await expr_cmp_ne_rat_int.expr_cmp_ne_rat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_ne_rat_nat', async () => {
      await expr_cmp_ne_rat_nat.expr_cmp_ne_rat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_ne_rat_nat.expr_cmp_ne_rat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_ne_rat_nat.expr_cmp_ne_rat_nat.exec({ as: alice })
      //      const res_after = await expr_cmp_ne_rat_nat.expr_cmp_ne_rat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_ne_rat_rat', async () => {
      await expr_cmp_ne_rat_rat.expr_cmp_ne_rat_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_ne_rat_rat.expr_cmp_ne_rat_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_ne_rat_rat.expr_cmp_ne_rat_rat.exec({ as: alice })
      //      const res_after = await expr_cmp_ne_rat_rat.expr_cmp_ne_rat_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_ne_str_str', async () => {
      await expr_cmp_ne_str_str.expr_cmp_ne_str_str.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_ne_str_str.expr_cmp_ne_str_str.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_ne_str_str.expr_cmp_ne_str_str.exec({ as: alice })
      //      const res_after = await expr_cmp_ne_str_str.expr_cmp_ne_str_str.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cmp_ne_tez_tez', async () => {
      await expr_cmp_ne_tez_tez.expr_cmp_ne_tez_tez.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cmp_ne_tez_tez.expr_cmp_ne_tez_tez.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cmp_ne_tez_tez.expr_cmp_ne_tez_tez.exec({ as: alice })
      //      const res_after = await expr_cmp_ne_tez_tez.expr_cmp_ne_tez_tez.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_control_fold', async () => {
      await expr_control_fold.expr_control_fold.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_control_fold.expr_control_fold.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_control_fold.expr_control_fold.exec({ as: alice })
      //      const res_after = await expr_control_fold.expr_control_fold.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_control_if_else_int_int', async () => {
      await expr_control_if_else_int_int.expr_control_if_else_int_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_control_if_else_int_int.expr_control_if_else_int_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_control_if_else_int_int.expr_control_if_else_int_int.exec({ as: alice })
      //      const res_after = await expr_control_if_else_int_int.expr_control_if_else_int_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_control_if_else_int_nat', async () => {
      await expr_control_if_else_int_nat.expr_control_if_else_int_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_control_if_else_int_nat.expr_control_if_else_int_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_control_if_else_int_nat.expr_control_if_else_int_nat.exec({ as: alice })
      //      const res_after = await expr_control_if_else_int_nat.expr_control_if_else_int_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_control_if_else_int_rat', async () => {
      await expr_control_if_else_int_rat.expr_control_if_else_int_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_control_if_else_int_rat.expr_control_if_else_int_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_control_if_else_int_rat.expr_control_if_else_int_rat.exec({ as: alice })
      //      const res_after = await expr_control_if_else_int_rat.expr_control_if_else_int_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_control_if_else_nat_int', async () => {
      await expr_control_if_else_nat_int.expr_control_if_else_nat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_control_if_else_nat_int.expr_control_if_else_nat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_control_if_else_nat_int.expr_control_if_else_nat_int.exec({ as: alice })
      //      const res_after = await expr_control_if_else_nat_int.expr_control_if_else_nat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_control_if_else_nat_nat', async () => {
      await expr_control_if_else_nat_nat.expr_control_if_else_nat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_control_if_else_nat_nat.expr_control_if_else_nat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_control_if_else_nat_nat.expr_control_if_else_nat_nat.exec({ as: alice })
      //      const res_after = await expr_control_if_else_nat_nat.expr_control_if_else_nat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_control_if_else_nat_rat', async () => {
      await expr_control_if_else_nat_rat.expr_control_if_else_nat_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_control_if_else_nat_rat.expr_control_if_else_nat_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_control_if_else_nat_rat.expr_control_if_else_nat_rat.exec({ as: alice })
      //      const res_after = await expr_control_if_else_nat_rat.expr_control_if_else_nat_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_control_if_else_rat_int', async () => {
      await expr_control_if_else_rat_int.expr_control_if_else_rat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_control_if_else_rat_int.expr_control_if_else_rat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_control_if_else_rat_int.expr_control_if_else_rat_int.exec({ as: alice })
      //      const res_after = await expr_control_if_else_rat_int.expr_control_if_else_rat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_control_if_else_rat_nat', async () => {
      await expr_control_if_else_rat_nat.expr_control_if_else_rat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_control_if_else_rat_nat.expr_control_if_else_rat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_control_if_else_rat_nat.expr_control_if_else_rat_nat.exec({ as: alice })
      //      const res_after = await expr_control_if_else_rat_nat.expr_control_if_else_rat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_control_if_else_rat_rat', async () => {
      await expr_control_if_else_rat_rat.expr_control_if_else_rat_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_control_if_else_rat_rat.expr_control_if_else_rat_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_control_if_else_rat_rat.expr_control_if_else_rat_rat.exec({ as: alice })
      //      const res_after = await expr_control_if_else_rat_rat.expr_control_if_else_rat_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_control_match_list', async () => {
      await expr_control_match_list.expr_control_match_list.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_control_match_list.expr_control_match_list.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_control_match_list.expr_control_match_list.exec({ as: alice })
      //      const res_after = await expr_control_match_list.expr_control_match_list.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_control_match_option', async () => {
      await expr_control_match_option.expr_control_match_option.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_control_match_option.expr_control_match_option.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_control_match_option.expr_control_match_option.exec({ as: alice })
      //      const res_after = await expr_control_match_option.expr_control_match_option.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_control_match_or', async () => {
      await expr_control_match_or.expr_control_match_or.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_control_match_or.expr_control_match_or.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_control_match_or.expr_control_match_or.exec({ as: alice })
      //      const res_after = await expr_control_match_or.expr_control_match_or.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_control_matchwith', async () => {
      await expr_control_matchwith.expr_control_matchwith.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_control_matchwith.expr_control_matchwith.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_control_matchwith.expr_control_matchwith.exec({ as: alice })
      //      const res_after = await expr_control_matchwith.expr_control_matchwith.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_control_matchwith_with_int_rat', async () => {
      await expr_control_matchwith_with_int_rat.expr_control_matchwith_with_int_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_control_matchwith_with_int_rat.expr_control_matchwith_with_int_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_control_matchwith_with_int_rat.expr_control_matchwith_with_int_rat.exec({ as: alice })
      //      const res_after = await expr_control_matchwith_with_int_rat.expr_control_matchwith_with_int_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_control_matchwith_with_nat_int', async () => {
      await expr_control_matchwith_with_nat_int.expr_control_matchwith_with_nat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_control_matchwith_with_nat_int.expr_control_matchwith_with_nat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_control_matchwith_with_nat_int.expr_control_matchwith_with_nat_int.exec({ as: alice })
      //      const res_after = await expr_control_matchwith_with_nat_int.expr_control_matchwith_with_nat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_control_matchwith_with_nat_rat', async () => {
      await expr_control_matchwith_with_nat_rat.expr_control_matchwith_with_nat_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_control_matchwith_with_nat_rat.expr_control_matchwith_with_nat_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_control_matchwith_with_nat_rat.expr_control_matchwith_with_nat_rat.exec({ as: alice })
      //      const res_after = await expr_control_matchwith_with_nat_rat.expr_control_matchwith_with_nat_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cst_balance', async () => {
      await expr_cst_balance.expr_cst_balance.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cst_balance.expr_cst_balance.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cst_balance.expr_cst_balance.exec({ as: alice })
      //      const res_after = await expr_cst_balance.expr_cst_balance.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cst_caller', async () => {
      await expr_cst_caller.expr_cst_caller.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cst_caller.expr_cst_caller.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cst_caller.expr_cst_caller.exec({ as: alice })
      //      const res_after = await expr_cst_caller.expr_cst_caller.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cst_level', async () => {
      await expr_cst_level.expr_cst_level.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cst_level.expr_cst_level.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cst_level.expr_cst_level.exec({ as: alice })
      //      const res_after = await expr_cst_level.expr_cst_level.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cst_min_block_time', async () => {
      await expr_cst_min_block_time.expr_cst_min_block_time.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cst_min_block_time.expr_cst_min_block_time.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cst_min_block_time.expr_cst_min_block_time.exec({ as: alice })
      //      const res_after = await expr_cst_min_block_time.expr_cst_min_block_time.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cst_now', async () => {
      await expr_cst_now.expr_cst_now.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cst_now.expr_cst_now.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cst_now.expr_cst_now.exec({ as: alice })
      //      const res_after = await expr_cst_now.expr_cst_now.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cst_self_address', async () => {
      await expr_cst_self_address.expr_cst_self_address.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cst_self_address.expr_cst_self_address.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cst_self_address.expr_cst_self_address.exec({ as: alice })
      //      const res_after = await expr_cst_self_address.expr_cst_self_address.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cst_self_chain_id', async () => {
      await expr_cst_self_chain_id.expr_cst_self_chain_id.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cst_self_chain_id.expr_cst_self_chain_id.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cst_self_chain_id.expr_cst_self_chain_id.exec({ as: alice })
      //      const res_after = await expr_cst_self_chain_id.expr_cst_self_chain_id.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cst_source', async () => {
      await expr_cst_source.expr_cst_source.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cst_source.expr_cst_source.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cst_source.expr_cst_source.exec({ as: alice })
      //      const res_after = await expr_cst_source.expr_cst_source.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cst_total_voting_power', async () => {
      await expr_cst_total_voting_power.expr_cst_total_voting_power.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cst_total_voting_power.expr_cst_total_voting_power.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cst_total_voting_power.expr_cst_total_voting_power.exec({ as: alice })
      //      const res_after = await expr_cst_total_voting_power.expr_cst_total_voting_power.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_cst_transferred', async () => {
      await expr_cst_transferred.expr_cst_transferred.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_cst_transferred.expr_cst_transferred.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_cst_transferred.expr_cst_transferred.exec({ as: alice })
      //      const res_after = await expr_cst_transferred.expr_cst_transferred.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fail_some_none', async () => {
      await expr_fail_some_none.expr_fail_some_none.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fail_some_none.expr_fail_some_none.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fail_some_none.expr_fail_some_none.exec({ as: alice })
      //      const res_after = await expr_fail_some_none.expr_fail_some_none.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fail_some_some', async () => {
      await expr_fail_some_some.expr_fail_some_some.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fail_some_some.expr_fail_some_some.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fail_some_some.expr_fail_some_some.exec({ as: alice })
      //      const res_after = await expr_fail_some_some.expr_fail_some_some.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_asset_method_contains', async () => {
      await expr_formula_asset_method_contains.expr_formula_asset_method_contains.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_asset_method_contains.expr_formula_asset_method_contains.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_asset_method_contains.expr_formula_asset_method_contains.exec({ as: alice })
      //      const res_after = await expr_formula_asset_method_contains.expr_formula_asset_method_contains.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_asset_method_count', async () => {
      await expr_formula_asset_method_count.expr_formula_asset_method_count.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_asset_method_count.expr_formula_asset_method_count.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_asset_method_count.expr_formula_asset_method_count.exec({ as: alice })
      //      const res_after = await expr_formula_asset_method_count.expr_formula_asset_method_count.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_asset_method_diff_view', async () => {
      await expr_formula_asset_method_diff_view.expr_formula_asset_method_diff_view.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_asset_method_diff_view.expr_formula_asset_method_diff_view.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_asset_method_diff_view.expr_formula_asset_method_diff_view.exec({ as: alice })
      //      const res_after = await expr_formula_asset_method_diff_view.expr_formula_asset_method_diff_view.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_asset_method_empty', async () => {
      await expr_formula_asset_method_empty.expr_formula_asset_method_empty.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_asset_method_empty.expr_formula_asset_method_empty.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_asset_method_empty.expr_formula_asset_method_empty.exec({ as: alice })
      //      const res_after = await expr_formula_asset_method_empty.expr_formula_asset_method_empty.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_asset_method_get', async () => {
      await expr_formula_asset_method_get.expr_formula_asset_method_get.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_asset_method_get.expr_formula_asset_method_get.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_asset_method_get.expr_formula_asset_method_get.exec({ as: alice })
      //      const res_after = await expr_formula_asset_method_get.expr_formula_asset_method_get.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_asset_method_head', async () => {
      await expr_formula_asset_method_head.expr_formula_asset_method_head.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_asset_method_head.expr_formula_asset_method_head.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_asset_method_head.expr_formula_asset_method_head.exec({ as: alice })
      //      const res_after = await expr_formula_asset_method_head.expr_formula_asset_method_head.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_asset_method_inter_view', async () => {
      await expr_formula_asset_method_inter_view.expr_formula_asset_method_inter_view.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_asset_method_inter_view.expr_formula_asset_method_inter_view.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_asset_method_inter_view.expr_formula_asset_method_inter_view.exec({ as: alice })
      //      const res_after = await expr_formula_asset_method_inter_view.expr_formula_asset_method_inter_view.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_asset_method_isempty', async () => {
      await expr_formula_asset_method_isempty.expr_formula_asset_method_isempty.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_asset_method_isempty.expr_formula_asset_method_isempty.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_asset_method_isempty.expr_formula_asset_method_isempty.exec({ as: alice })
      //      const res_after = await expr_formula_asset_method_isempty.expr_formula_asset_method_isempty.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_asset_method_nth', async () => {
      await expr_formula_asset_method_nth.expr_formula_asset_method_nth.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_asset_method_nth.expr_formula_asset_method_nth.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_asset_method_nth.expr_formula_asset_method_nth.exec({ as: alice })
      //      const res_after = await expr_formula_asset_method_nth.expr_formula_asset_method_nth.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_asset_method_select', async () => {
      await expr_formula_asset_method_select.expr_formula_asset_method_select.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_asset_method_select.expr_formula_asset_method_select.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_asset_method_select.expr_formula_asset_method_select.exec({ as: alice })
      //      const res_after = await expr_formula_asset_method_select.expr_formula_asset_method_select.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_asset_method_singleton', async () => {
      await expr_formula_asset_method_singleton.expr_formula_asset_method_singleton.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_asset_method_singleton.expr_formula_asset_method_singleton.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_asset_method_singleton.expr_formula_asset_method_singleton.exec({ as: alice })
      //      const res_after = await expr_formula_asset_method_singleton.expr_formula_asset_method_singleton.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_asset_method_subsetof_aggregate', async () => {
      await expr_formula_asset_method_subsetof_aggregate.expr_formula_asset_method_subsetof_aggregate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_asset_method_subsetof_aggregate.expr_formula_asset_method_subsetof_aggregate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_asset_method_subsetof_aggregate.expr_formula_asset_method_subsetof_aggregate.exec({ as: alice })
      //      const res_after = await expr_formula_asset_method_subsetof_aggregate.expr_formula_asset_method_subsetof_aggregate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_asset_method_subsetof_collection', async () => {
      await expr_formula_asset_method_subsetof_collection.expr_formula_asset_method_subsetof_collection.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_asset_method_subsetof_collection.expr_formula_asset_method_subsetof_collection.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_asset_method_subsetof_collection.expr_formula_asset_method_subsetof_collection.exec({ as: alice })
      //      const res_after = await expr_formula_asset_method_subsetof_collection.expr_formula_asset_method_subsetof_collection.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_asset_method_subsetof_partition', async () => {
      await expr_formula_asset_method_subsetof_partition.expr_formula_asset_method_subsetof_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_asset_method_subsetof_partition.expr_formula_asset_method_subsetof_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_asset_method_subsetof_partition.expr_formula_asset_method_subsetof_partition.exec({ as: alice })
      //      const res_after = await expr_formula_asset_method_subsetof_partition.expr_formula_asset_method_subsetof_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_asset_method_subsetof_view', async () => {
      await expr_formula_asset_method_subsetof_view.expr_formula_asset_method_subsetof_view.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_asset_method_subsetof_view.expr_formula_asset_method_subsetof_view.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_asset_method_subsetof_view.expr_formula_asset_method_subsetof_view.exec({ as: alice })
      //      const res_after = await expr_formula_asset_method_subsetof_view.expr_formula_asset_method_subsetof_view.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_asset_method_sum', async () => {
      await expr_formula_asset_method_sum.expr_formula_asset_method_sum.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_asset_method_sum.expr_formula_asset_method_sum.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_asset_method_sum.expr_formula_asset_method_sum.exec({ as: alice })
      //      const res_after = await expr_formula_asset_method_sum.expr_formula_asset_method_sum.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_asset_method_tail', async () => {
      await expr_formula_asset_method_tail.expr_formula_asset_method_tail.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_asset_method_tail.expr_formula_asset_method_tail.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_asset_method_tail.expr_formula_asset_method_tail.exec({ as: alice })
      //      const res_after = await expr_formula_asset_method_tail.expr_formula_asset_method_tail.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_asset_method_union_view', async () => {
      await expr_formula_asset_method_union_view.expr_formula_asset_method_union_view.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_asset_method_union_view.expr_formula_asset_method_union_view.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_asset_method_union_view.expr_formula_asset_method_union_view.exec({ as: alice })
      //      const res_after = await expr_formula_asset_method_union_view.expr_formula_asset_method_union_view.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_at', async () => {
      await expr_formula_at.expr_formula_at.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_at.expr_formula_at.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_at.expr_formula_at.exec({ as: alice })
      //      const res_after = await expr_formula_at.expr_formula_at.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_before', async () => {
      await expr_formula_before.expr_formula_before.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_before.expr_formula_before.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_before.expr_formula_before.exec({ as: alice })
      //      const res_after = await expr_formula_before.expr_formula_before.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_cmp_eq_list', async () => {
      await expr_formula_cmp_eq_list.expr_formula_cmp_eq_list.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_cmp_eq_list.expr_formula_cmp_eq_list.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_cmp_eq_list.expr_formula_cmp_eq_list.exec({ as: alice })
      //      const res_after = await expr_formula_cmp_eq_list.expr_formula_cmp_eq_list.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_cmp_eq_map', async () => {
      await expr_formula_cmp_eq_map.expr_formula_cmp_eq_map.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_cmp_eq_map.expr_formula_cmp_eq_map.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_cmp_eq_map.expr_formula_cmp_eq_map.exec({ as: alice })
      //      const res_after = await expr_formula_cmp_eq_map.expr_formula_cmp_eq_map.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_cmp_eq_option', async () => {
      await expr_formula_cmp_eq_option.expr_formula_cmp_eq_option.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_cmp_eq_option.expr_formula_cmp_eq_option.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_cmp_eq_option.expr_formula_cmp_eq_option.exec({ as: alice })
      //      const res_after = await expr_formula_cmp_eq_option.expr_formula_cmp_eq_option.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_cmp_eq_set', async () => {
      await expr_formula_cmp_eq_set.expr_formula_cmp_eq_set.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_cmp_eq_set.expr_formula_cmp_eq_set.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_cmp_eq_set.expr_formula_cmp_eq_set.exec({ as: alice })
      //      const res_after = await expr_formula_cmp_eq_set.expr_formula_cmp_eq_set.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_cmp_ne_list', async () => {
      await expr_formula_cmp_ne_list.expr_formula_cmp_ne_list.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_cmp_ne_list.expr_formula_cmp_ne_list.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_cmp_ne_list.expr_formula_cmp_ne_list.exec({ as: alice })
      //      const res_after = await expr_formula_cmp_ne_list.expr_formula_cmp_ne_list.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_cmp_ne_map', async () => {
      await expr_formula_cmp_ne_map.expr_formula_cmp_ne_map.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_cmp_ne_map.expr_formula_cmp_ne_map.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_cmp_ne_map.expr_formula_cmp_ne_map.exec({ as: alice })
      //      const res_after = await expr_formula_cmp_ne_map.expr_formula_cmp_ne_map.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_cmp_ne_option', async () => {
      await expr_formula_cmp_ne_option.expr_formula_cmp_ne_option.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_cmp_ne_option.expr_formula_cmp_ne_option.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_cmp_ne_option.expr_formula_cmp_ne_option.exec({ as: alice })
      //      const res_after = await expr_formula_cmp_ne_option.expr_formula_cmp_ne_option.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_cmp_ne_set', async () => {
      await expr_formula_cmp_ne_set.expr_formula_cmp_ne_set.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_cmp_ne_set.expr_formula_cmp_ne_set.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_cmp_ne_set.expr_formula_cmp_ne_set.exec({ as: alice })
      //      const res_after = await expr_formula_cmp_ne_set.expr_formula_cmp_ne_set.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_equiv', async () => {
      await expr_formula_equiv.expr_formula_equiv.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_equiv.expr_formula_equiv.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_equiv.expr_formula_equiv.exec({ as: alice })
      //      const res_after = await expr_formula_equiv.expr_formula_equiv.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_exists_asset', async () => {
      await expr_formula_exists_asset.expr_formula_exists_asset.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_exists_asset.expr_formula_exists_asset.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_exists_asset.expr_formula_exists_asset.exec({ as: alice })
      //      const res_after = await expr_formula_exists_asset.expr_formula_exists_asset.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_exists_builtin', async () => {
      await expr_formula_exists_builtin.expr_formula_exists_builtin.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_exists_builtin.expr_formula_exists_builtin.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_exists_builtin.expr_formula_exists_builtin.exec({ as: alice })
      //      const res_after = await expr_formula_exists_builtin.expr_formula_exists_builtin.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_forall_asset', async () => {
      await expr_formula_forall_asset.expr_formula_forall_asset.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_forall_asset.expr_formula_forall_asset.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_forall_asset.expr_formula_forall_asset.exec({ as: alice })
      //      const res_after = await expr_formula_forall_asset.expr_formula_forall_asset.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_forall_builtin', async () => {
      await expr_formula_forall_builtin.expr_formula_forall_builtin.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_forall_builtin.expr_formula_forall_builtin.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_forall_builtin.expr_formula_forall_builtin.exec({ as: alice })
      //      const res_after = await expr_formula_forall_builtin.expr_formula_forall_builtin.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_implication', async () => {
      await expr_formula_implication.expr_formula_implication.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_implication.expr_formula_implication.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_implication.expr_formula_implication.exec({ as: alice })
      //      const res_after = await expr_formula_implication.expr_formula_implication.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_iterated_aggregate', async () => {
      await expr_formula_iterated_aggregate.expr_formula_iterated_aggregate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_iterated_aggregate.expr_formula_iterated_aggregate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_iterated_aggregate.expr_formula_iterated_aggregate.exec({ as: alice })
      //      const res_after = await expr_formula_iterated_aggregate.expr_formula_iterated_aggregate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_iterated_collection', async () => {
      await expr_formula_iterated_collection.expr_formula_iterated_collection.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_iterated_collection.expr_formula_iterated_collection.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_iterated_collection.expr_formula_iterated_collection.exec({ as: alice })
      //      const res_after = await expr_formula_iterated_collection.expr_formula_iterated_collection.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_iterated_partition', async () => {
      await expr_formula_iterated_partition.expr_formula_iterated_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_iterated_partition.expr_formula_iterated_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_iterated_partition.expr_formula_iterated_partition.exec({ as: alice })
      //      const res_after = await expr_formula_iterated_partition.expr_formula_iterated_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_iterated_view', async () => {
      await expr_formula_iterated_view.expr_formula_iterated_view.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_iterated_view.expr_formula_iterated_view.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_iterated_view.expr_formula_iterated_view.exec({ as: alice })
      //      const res_after = await expr_formula_iterated_view.expr_formula_iterated_view.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_toiterate_aggregate', async () => {
      await expr_formula_toiterate_aggregate.expr_formula_toiterate_aggregate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_toiterate_aggregate.expr_formula_toiterate_aggregate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_toiterate_aggregate.expr_formula_toiterate_aggregate.exec({ as: alice })
      //      const res_after = await expr_formula_toiterate_aggregate.expr_formula_toiterate_aggregate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_toiterate_collection', async () => {
      await expr_formula_toiterate_collection.expr_formula_toiterate_collection.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_toiterate_collection.expr_formula_toiterate_collection.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_toiterate_collection.expr_formula_toiterate_collection.exec({ as: alice })
      //      const res_after = await expr_formula_toiterate_collection.expr_formula_toiterate_collection.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_toiterate_partition', async () => {
      await expr_formula_toiterate_partition.expr_formula_toiterate_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_toiterate_partition.expr_formula_toiterate_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_toiterate_partition.expr_formula_toiterate_partition.exec({ as: alice })
      //      const res_after = await expr_formula_toiterate_partition.expr_formula_toiterate_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_formula_toiterate_view', async () => {
      await expr_formula_toiterate_view.expr_formula_toiterate_view.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_formula_toiterate_view.expr_formula_toiterate_view.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_formula_toiterate_view.expr_formula_toiterate_view.exec({ as: alice })
      //      const res_after = await expr_formula_toiterate_view.expr_formula_toiterate_view.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_abs_int', async () => {
      await expr_fun_abs_int.expr_fun_abs_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_abs_int.expr_fun_abs_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_abs_int.expr_fun_abs_int.exec({ as: alice })
      //      const res_after = await expr_fun_abs_int.expr_fun_abs_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_abs_rat', async () => {
      await expr_fun_abs_rat.expr_fun_abs_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_abs_rat.expr_fun_abs_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_abs_rat.expr_fun_abs_rat.exec({ as: alice })
      //      const res_after = await expr_fun_abs_rat.expr_fun_abs_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_address_to_contract', async () => {
      await expr_fun_address_to_contract.expr_fun_address_to_contract.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_address_to_contract.expr_fun_address_to_contract.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_address_to_contract.expr_fun_address_to_contract.exec({ as: alice })
      //      const res_after = await expr_fun_address_to_contract.expr_fun_address_to_contract.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_address_to_contract_unit', async () => {
      await expr_fun_address_to_contract_unit.expr_fun_address_to_contract_unit.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_address_to_contract_unit.expr_fun_address_to_contract_unit.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_address_to_contract_unit.expr_fun_address_to_contract_unit.exec({ as: alice })
      //      const res_after = await expr_fun_address_to_contract_unit.expr_fun_address_to_contract_unit.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_ceil', async () => {
      await expr_fun_ceil.expr_fun_ceil.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_ceil.expr_fun_ceil.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_ceil.expr_fun_ceil.exec({ as: alice })
      //      const res_after = await expr_fun_ceil.expr_fun_ceil.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_concat_byt', async () => {
      await expr_fun_concat_byt.expr_fun_concat_byt.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_concat_byt.expr_fun_concat_byt.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_concat_byt.expr_fun_concat_byt.exec({ as: alice })
      //      const res_after = await expr_fun_concat_byt.expr_fun_concat_byt.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_concat_list_byt', async () => {
      await expr_fun_concat_list_byt.expr_fun_concat_list_byt.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_concat_list_byt.expr_fun_concat_list_byt.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_concat_list_byt.expr_fun_concat_list_byt.exec({ as: alice })
      //      const res_after = await expr_fun_concat_list_byt.expr_fun_concat_list_byt.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_concat_list_str', async () => {
      await expr_fun_concat_list_str.expr_fun_concat_list_str.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_concat_list_str.expr_fun_concat_list_str.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_concat_list_str.expr_fun_concat_list_str.exec({ as: alice })
      //      const res_after = await expr_fun_concat_list_str.expr_fun_concat_list_str.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_concat_str', async () => {
      await expr_fun_concat_str.expr_fun_concat_str.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_concat_str.expr_fun_concat_str.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_concat_str.expr_fun_concat_str.exec({ as: alice })
      //      const res_after = await expr_fun_concat_str.expr_fun_concat_str.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_floor', async () => {
      await expr_fun_floor.expr_fun_floor.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_floor.expr_fun_floor.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_floor.expr_fun_floor.exec({ as: alice })
      //      const res_after = await expr_fun_floor.expr_fun_floor.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_int_to_nat', async () => {
      await expr_fun_int_to_nat.expr_fun_int_to_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_int_to_nat.expr_fun_int_to_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_int_to_nat.expr_fun_int_to_nat.exec({ as: alice })
      //      const res_after = await expr_fun_int_to_nat.expr_fun_int_to_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_key_hash_to_contract', async () => {
      await expr_fun_key_hash_to_contract.expr_fun_key_hash_to_contract.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_key_hash_to_contract.expr_fun_key_hash_to_contract.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_key_hash_to_contract.expr_fun_key_hash_to_contract.exec({ as: alice })
      //      const res_after = await expr_fun_key_hash_to_contract.expr_fun_key_hash_to_contract.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_length_bytes', async () => {
      await expr_fun_length_bytes.expr_fun_length_bytes.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_length_bytes.expr_fun_length_bytes.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_length_bytes.expr_fun_length_bytes.exec({ as: alice })
      //      const res_after = await expr_fun_length_bytes.expr_fun_length_bytes.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_length_str', async () => {
      await expr_fun_length_str.expr_fun_length_str.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_length_str.expr_fun_length_str.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_length_str.expr_fun_length_str.exec({ as: alice })
      //      const res_after = await expr_fun_length_str.expr_fun_length_str.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_make_event', async () => {
      await expr_fun_make_event.expr_fun_make_event.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_make_event.expr_fun_make_event.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_make_event.expr_fun_make_event.exec({ as: alice })
      //      const res_after = await expr_fun_make_event.expr_fun_make_event.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_make_operation', async () => {
      await expr_fun_make_operation.expr_fun_make_operation.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_make_operation.expr_fun_make_operation.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_make_operation.expr_fun_make_operation.exec({ as: alice })
      //      const res_after = await expr_fun_make_operation.expr_fun_make_operation.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_max_date', async () => {
      await expr_fun_max_date.expr_fun_max_date.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_max_date.expr_fun_max_date.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_max_date.expr_fun_max_date.exec({ as: alice })
      //      const res_after = await expr_fun_max_date.expr_fun_max_date.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_max_dur', async () => {
      await expr_fun_max_dur.expr_fun_max_dur.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_max_dur.expr_fun_max_dur.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_max_dur.expr_fun_max_dur.exec({ as: alice })
      //      const res_after = await expr_fun_max_dur.expr_fun_max_dur.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_max_int_int', async () => {
      await expr_fun_max_int_int.expr_fun_max_int_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_max_int_int.expr_fun_max_int_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_max_int_int.expr_fun_max_int_int.exec({ as: alice })
      //      const res_after = await expr_fun_max_int_int.expr_fun_max_int_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_max_int_nat', async () => {
      await expr_fun_max_int_nat.expr_fun_max_int_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_max_int_nat.expr_fun_max_int_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_max_int_nat.expr_fun_max_int_nat.exec({ as: alice })
      //      const res_after = await expr_fun_max_int_nat.expr_fun_max_int_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_max_int_rat', async () => {
      await expr_fun_max_int_rat.expr_fun_max_int_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_max_int_rat.expr_fun_max_int_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_max_int_rat.expr_fun_max_int_rat.exec({ as: alice })
      //      const res_after = await expr_fun_max_int_rat.expr_fun_max_int_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_max_nat_int', async () => {
      await expr_fun_max_nat_int.expr_fun_max_nat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_max_nat_int.expr_fun_max_nat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_max_nat_int.expr_fun_max_nat_int.exec({ as: alice })
      //      const res_after = await expr_fun_max_nat_int.expr_fun_max_nat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_max_nat_nat', async () => {
      await expr_fun_max_nat_nat.expr_fun_max_nat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_max_nat_nat.expr_fun_max_nat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_max_nat_nat.expr_fun_max_nat_nat.exec({ as: alice })
      //      const res_after = await expr_fun_max_nat_nat.expr_fun_max_nat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_max_nat_rat', async () => {
      await expr_fun_max_nat_rat.expr_fun_max_nat_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_max_nat_rat.expr_fun_max_nat_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_max_nat_rat.expr_fun_max_nat_rat.exec({ as: alice })
      //      const res_after = await expr_fun_max_nat_rat.expr_fun_max_nat_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_max_rat_int', async () => {
      await expr_fun_max_rat_int.expr_fun_max_rat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_max_rat_int.expr_fun_max_rat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_max_rat_int.expr_fun_max_rat_int.exec({ as: alice })
      //      const res_after = await expr_fun_max_rat_int.expr_fun_max_rat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_max_rat_nat', async () => {
      await expr_fun_max_rat_nat.expr_fun_max_rat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_max_rat_nat.expr_fun_max_rat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_max_rat_nat.expr_fun_max_rat_nat.exec({ as: alice })
      //      const res_after = await expr_fun_max_rat_nat.expr_fun_max_rat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_max_rat_rat', async () => {
      await expr_fun_max_rat_rat.expr_fun_max_rat_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_max_rat_rat.expr_fun_max_rat_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_max_rat_rat.expr_fun_max_rat_rat.exec({ as: alice })
      //      const res_after = await expr_fun_max_rat_rat.expr_fun_max_rat_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_max_tez', async () => {
      await expr_fun_max_tez.expr_fun_max_tez.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_max_tez.expr_fun_max_tez.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_max_tez.expr_fun_max_tez.exec({ as: alice })
      //      const res_after = await expr_fun_max_tez.expr_fun_max_tez.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_min_date', async () => {
      await expr_fun_min_date.expr_fun_min_date.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_min_date.expr_fun_min_date.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_min_date.expr_fun_min_date.exec({ as: alice })
      //      const res_after = await expr_fun_min_date.expr_fun_min_date.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_min_dur', async () => {
      await expr_fun_min_dur.expr_fun_min_dur.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_min_dur.expr_fun_min_dur.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_min_dur.expr_fun_min_dur.exec({ as: alice })
      //      const res_after = await expr_fun_min_dur.expr_fun_min_dur.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_min_int_int', async () => {
      await expr_fun_min_int_int.expr_fun_min_int_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_min_int_int.expr_fun_min_int_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_min_int_int.expr_fun_min_int_int.exec({ as: alice })
      //      const res_after = await expr_fun_min_int_int.expr_fun_min_int_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_min_int_nat', async () => {
      await expr_fun_min_int_nat.expr_fun_min_int_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_min_int_nat.expr_fun_min_int_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_min_int_nat.expr_fun_min_int_nat.exec({ as: alice })
      //      const res_after = await expr_fun_min_int_nat.expr_fun_min_int_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_min_int_rat', async () => {
      await expr_fun_min_int_rat.expr_fun_min_int_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_min_int_rat.expr_fun_min_int_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_min_int_rat.expr_fun_min_int_rat.exec({ as: alice })
      //      const res_after = await expr_fun_min_int_rat.expr_fun_min_int_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_min_nat_int', async () => {
      await expr_fun_min_nat_int.expr_fun_min_nat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_min_nat_int.expr_fun_min_nat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_min_nat_int.expr_fun_min_nat_int.exec({ as: alice })
      //      const res_after = await expr_fun_min_nat_int.expr_fun_min_nat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_min_nat_nat', async () => {
      await expr_fun_min_nat_nat.expr_fun_min_nat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_min_nat_nat.expr_fun_min_nat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_min_nat_nat.expr_fun_min_nat_nat.exec({ as: alice })
      //      const res_after = await expr_fun_min_nat_nat.expr_fun_min_nat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_min_nat_rat', async () => {
      await expr_fun_min_nat_rat.expr_fun_min_nat_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_min_nat_rat.expr_fun_min_nat_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_min_nat_rat.expr_fun_min_nat_rat.exec({ as: alice })
      //      const res_after = await expr_fun_min_nat_rat.expr_fun_min_nat_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_min_rat_int', async () => {
      await expr_fun_min_rat_int.expr_fun_min_rat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_min_rat_int.expr_fun_min_rat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_min_rat_int.expr_fun_min_rat_int.exec({ as: alice })
      //      const res_after = await expr_fun_min_rat_int.expr_fun_min_rat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_min_rat_nat', async () => {
      await expr_fun_min_rat_nat.expr_fun_min_rat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_min_rat_nat.expr_fun_min_rat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_min_rat_nat.expr_fun_min_rat_nat.exec({ as: alice })
      //      const res_after = await expr_fun_min_rat_nat.expr_fun_min_rat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_min_rat_rat', async () => {
      await expr_fun_min_rat_rat.expr_fun_min_rat_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_min_rat_rat.expr_fun_min_rat_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_min_rat_rat.expr_fun_min_rat_rat.exec({ as: alice })
      //      const res_after = await expr_fun_min_rat_rat.expr_fun_min_rat_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_min_tez', async () => {
      await expr_fun_min_tez.expr_fun_min_tez.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_min_tez.expr_fun_min_tez.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_min_tez.expr_fun_min_tez.exec({ as: alice })
      //      const res_after = await expr_fun_min_tez.expr_fun_min_tez.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_nat_to_string', async () => {
      await expr_fun_nat_to_string.expr_fun_nat_to_string.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_nat_to_string.expr_fun_nat_to_string.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_nat_to_string.expr_fun_nat_to_string.exec({ as: alice })
      //      const res_after = await expr_fun_nat_to_string.expr_fun_nat_to_string.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_opt_get_some', async () => {
      await expr_fun_opt_get_some.expr_fun_opt_get_some.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_opt_get_some.expr_fun_opt_get_some.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_opt_get_some.expr_fun_opt_get_some.exec({ as: alice })
      //      const res_after = await expr_fun_opt_get_some.expr_fun_opt_get_some.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_opt_is_none', async () => {
      await expr_fun_opt_is_none.expr_fun_opt_is_none.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_opt_is_none.expr_fun_opt_is_none.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_opt_is_none.expr_fun_opt_is_none.exec({ as: alice })
      //      const res_after = await expr_fun_opt_is_none.expr_fun_opt_is_none.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_opt_is_some', async () => {
      await expr_fun_opt_is_some.expr_fun_opt_is_some.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_opt_is_some.expr_fun_opt_is_some.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_opt_is_some.expr_fun_opt_is_some.exec({ as: alice })
      //      const res_after = await expr_fun_opt_is_some.expr_fun_opt_is_some.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_opt_require_some', async () => {
      await expr_fun_opt_require_some.expr_fun_opt_require_some.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_opt_require_some.expr_fun_opt_require_some.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_opt_require_some.expr_fun_opt_require_some.exec({ as: alice })
      //      const res_after = await expr_fun_opt_require_some.expr_fun_opt_require_some.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_pack_complex', async () => {
      await expr_fun_pack_complex.expr_fun_pack_complex.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_pack_complex.expr_fun_pack_complex.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_pack_complex.expr_fun_pack_complex.exec({ as: alice })
      //      const res_after = await expr_fun_pack_complex.expr_fun_pack_complex.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_pack_lit_tuple', async () => {
      await expr_fun_pack_lit_tuple.expr_fun_pack_lit_tuple.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_pack_lit_tuple.expr_fun_pack_lit_tuple.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_pack_lit_tuple.expr_fun_pack_lit_tuple.exec({ as: alice })
      //      const res_after = await expr_fun_pack_lit_tuple.expr_fun_pack_lit_tuple.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_pack_string', async () => {
      await expr_fun_pack_string.expr_fun_pack_string.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_pack_string.expr_fun_pack_string.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_pack_string.expr_fun_pack_string.exec({ as: alice })
      //      const res_after = await expr_fun_pack_string.expr_fun_pack_string.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_setdelegate', async () => {
      await expr_fun_setdelegate.expr_fun_setdelegate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_setdelegate.expr_fun_setdelegate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_setdelegate.expr_fun_setdelegate.exec({ as: alice })
      //      const res_after = await expr_fun_setdelegate.expr_fun_setdelegate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_slice_byt', async () => {
      await expr_fun_slice_byt.expr_fun_slice_byt.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_slice_byt.expr_fun_slice_byt.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_slice_byt.expr_fun_slice_byt.exec({ as: alice })
      //      const res_after = await expr_fun_slice_byt.expr_fun_slice_byt.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_slice_str', async () => {
      await expr_fun_slice_str.expr_fun_slice_str.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_slice_str.expr_fun_slice_str.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_slice_str.expr_fun_slice_str.exec({ as: alice })
      //      const res_after = await expr_fun_slice_str.expr_fun_slice_str.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_sub_mutez', async () => {
      await expr_fun_sub_mutez.expr_fun_sub_mutez.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_sub_mutez.expr_fun_sub_mutez.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_sub_mutez.expr_fun_sub_mutez.exec({ as: alice })
      //      const res_after = await expr_fun_sub_mutez.expr_fun_sub_mutez.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_sub_nat', async () => {
      await expr_fun_sub_nat.expr_fun_sub_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_sub_nat.expr_fun_sub_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_sub_nat.expr_fun_sub_nat.exec({ as: alice })
      //      const res_after = await expr_fun_sub_nat.expr_fun_sub_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_sub_nat_zero', async () => {
      await expr_fun_sub_nat_zero.expr_fun_sub_nat_zero.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_sub_nat_zero.expr_fun_sub_nat_zero.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_sub_nat_zero.expr_fun_sub_nat_zero.exec({ as: alice })
      //      const res_after = await expr_fun_sub_nat_zero.expr_fun_sub_nat_zero.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_unpack_bool', async () => {
      await expr_fun_unpack_bool.expr_fun_unpack_bool.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_unpack_bool.expr_fun_unpack_bool.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_unpack_bool.expr_fun_unpack_bool.exec({ as: alice })
      //      const res_after = await expr_fun_unpack_bool.expr_fun_unpack_bool.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_unpack_complex', async () => {
      await expr_fun_unpack_complex.expr_fun_unpack_complex.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_unpack_complex.expr_fun_unpack_complex.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_unpack_complex.expr_fun_unpack_complex.exec({ as: alice })
      //      const res_after = await expr_fun_unpack_complex.expr_fun_unpack_complex.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_fun_unpack_string', async () => {
      await expr_fun_unpack_string.expr_fun_unpack_string.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_fun_unpack_string.expr_fun_unpack_string.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_fun_unpack_string.expr_fun_unpack_string.exec({ as: alice })
      //      const res_after = await expr_fun_unpack_string.expr_fun_unpack_string.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_instr_rec_1_0', async () => {
      await expr_instr_rec_1_0.expr_instr_rec_1_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_instr_rec_1_0.expr_instr_rec_1_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_instr_rec_1_0.expr_instr_rec_1_0.exec({ as: alice })
      //      const res_after = await expr_instr_rec_1_0.expr_instr_rec_1_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_instr_rec_2_0', async () => {
      await expr_instr_rec_2_0.expr_instr_rec_2_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_instr_rec_2_0.expr_instr_rec_2_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_instr_rec_2_0.expr_instr_rec_2_0.exec({ as: alice })
      //      const res_after = await expr_instr_rec_2_0.expr_instr_rec_2_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_instr_rec_2_1', async () => {
      await expr_instr_rec_2_1.expr_instr_rec_2_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_instr_rec_2_1.expr_instr_rec_2_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_instr_rec_2_1.expr_instr_rec_2_1.exec({ as: alice })
      //      const res_after = await expr_instr_rec_2_1.expr_instr_rec_2_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_instr_rec_3_0', async () => {
      await expr_instr_rec_3_0.expr_instr_rec_3_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_instr_rec_3_0.expr_instr_rec_3_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_instr_rec_3_0.expr_instr_rec_3_0.exec({ as: alice })
      //      const res_after = await expr_instr_rec_3_0.expr_instr_rec_3_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_instr_rec_3_1', async () => {
      await expr_instr_rec_3_1.expr_instr_rec_3_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_instr_rec_3_1.expr_instr_rec_3_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_instr_rec_3_1.expr_instr_rec_3_1.exec({ as: alice })
      //      const res_after = await expr_instr_rec_3_1.expr_instr_rec_3_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_instr_rec_3_2', async () => {
      await expr_instr_rec_3_2.expr_instr_rec_3_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_instr_rec_3_2.expr_instr_rec_3_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_instr_rec_3_2.expr_instr_rec_3_2.exec({ as: alice })
      //      const res_after = await expr_instr_rec_3_2.expr_instr_rec_3_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_instr_rec_4_0', async () => {
      await expr_instr_rec_4_0.expr_instr_rec_4_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_instr_rec_4_0.expr_instr_rec_4_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_instr_rec_4_0.expr_instr_rec_4_0.exec({ as: alice })
      //      const res_after = await expr_instr_rec_4_0.expr_instr_rec_4_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_instr_rec_4_1', async () => {
      await expr_instr_rec_4_1.expr_instr_rec_4_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_instr_rec_4_1.expr_instr_rec_4_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_instr_rec_4_1.expr_instr_rec_4_1.exec({ as: alice })
      //      const res_after = await expr_instr_rec_4_1.expr_instr_rec_4_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_instr_rec_4_2', async () => {
      await expr_instr_rec_4_2.expr_instr_rec_4_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_instr_rec_4_2.expr_instr_rec_4_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_instr_rec_4_2.expr_instr_rec_4_2.exec({ as: alice })
      //      const res_after = await expr_instr_rec_4_2.expr_instr_rec_4_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_instr_rec_4_3', async () => {
      await expr_instr_rec_4_3.expr_instr_rec_4_3.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_instr_rec_4_3.expr_instr_rec_4_3.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_instr_rec_4_3.expr_instr_rec_4_3.exec({ as: alice })
      //      const res_after = await expr_instr_rec_4_3.expr_instr_rec_4_3.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_instr_rec_rollback', async () => {
      await expr_instr_rec_rollback.expr_instr_rec_rollback.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_instr_rec_rollback.expr_instr_rec_rollback.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_instr_rec_rollback.expr_instr_rec_rollback.exec({ as: alice })
      //      const res_after = await expr_instr_rec_rollback.expr_instr_rec_rollback.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_lambda', async () => {
      await expr_lambda.expr_lambda.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_lambda.expr_lambda.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_lambda.expr_lambda.exec({ as: alice })
      //      const res_after = await expr_lambda.expr_lambda.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_lambda2', async () => {
      await expr_lambda2.expr_lambda2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_lambda2.expr_lambda2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_lambda2.expr_lambda2.exec({ as: alice })
      //      const res_after = await expr_lambda2.expr_lambda2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_list_concat', async () => {
      await expr_list_concat.expr_list_concat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_list_concat.expr_list_concat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_list_concat.expr_list_concat.exec({ as: alice })
      //      const res_after = await expr_list_concat.expr_list_concat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_list_contains', async () => {
      await expr_list_contains.expr_list_contains.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_list_contains.expr_list_contains.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_list_contains.expr_list_contains.exec({ as: alice })
      //      const res_after = await expr_list_contains.expr_list_contains.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_list_length', async () => {
      await expr_list_length.expr_list_length.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_list_length.expr_list_length.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_list_length.expr_list_length.exec({ as: alice })
      //      const res_after = await expr_list_length.expr_list_length.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_list_lit', async () => {
      await expr_list_lit.expr_list_lit.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_list_lit.expr_list_lit.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_list_lit.expr_list_lit.exec({ as: alice })
      //      const res_after = await expr_list_lit.expr_list_lit.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_list_map', async () => {
      await expr_list_map.expr_list_map.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_list_map.expr_list_map.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_list_map.expr_list_map.exec({ as: alice })
      //      const res_after = await expr_list_map.expr_list_map.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_list_map_string_nat', async () => {
      await expr_list_map_string_nat.expr_list_map_string_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_list_map_string_nat.expr_list_map_string_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_list_map_string_nat.expr_list_map_string_nat.exec({ as: alice })
      //      const res_after = await expr_list_map_string_nat.expr_list_map_string_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_list_nth', async () => {
      await expr_list_nth.expr_list_nth.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_list_nth.expr_list_nth.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_list_nth.expr_list_nth.exec({ as: alice })
      //      const res_after = await expr_list_nth.expr_list_nth.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_list_prepend', async () => {
      await expr_list_prepend.expr_list_prepend.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_list_prepend.expr_list_prepend.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_list_prepend.expr_list_prepend.exec({ as: alice })
      //      const res_after = await expr_list_prepend.expr_list_prepend.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_list_reverse', async () => {
      await expr_list_reverse.expr_list_reverse.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_list_reverse.expr_list_reverse.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_list_reverse.expr_list_reverse.exec({ as: alice })
      //      const res_after = await expr_list_reverse.expr_list_reverse.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_lit_addr', async () => {
      await expr_lit_addr.expr_lit_addr.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_lit_addr.expr_lit_addr.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_lit_addr.expr_lit_addr.exec({ as: alice })
      //      const res_after = await expr_lit_addr.expr_lit_addr.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_lit_bytes', async () => {
      await expr_lit_bytes.expr_lit_bytes.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_lit_bytes.expr_lit_bytes.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_lit_bytes.expr_lit_bytes.exec({ as: alice })
      //      const res_after = await expr_lit_bytes.expr_lit_bytes.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_lit_cur_mtz', async () => {
      await expr_lit_cur_mtz.expr_lit_cur_mtz.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_lit_cur_mtz.expr_lit_cur_mtz.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_lit_cur_mtz.expr_lit_cur_mtz.exec({ as: alice })
      //      const res_after = await expr_lit_cur_mtz.expr_lit_cur_mtz.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_lit_cur_tz', async () => {
      await expr_lit_cur_tz.expr_lit_cur_tz.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_lit_cur_tz.expr_lit_cur_tz.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_lit_cur_tz.expr_lit_cur_tz.exec({ as: alice })
      //      const res_after = await expr_lit_cur_tz.expr_lit_cur_tz.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_lit_cur_utz', async () => {
      await expr_lit_cur_utz.expr_lit_cur_utz.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_lit_cur_utz.expr_lit_cur_utz.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_lit_cur_utz.expr_lit_cur_utz.exec({ as: alice })
      //      const res_after = await expr_lit_cur_utz.expr_lit_cur_utz.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_lit_date_0', async () => {
      await expr_lit_date_0.expr_lit_date_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_lit_date_0.expr_lit_date_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_lit_date_0.expr_lit_date_0.exec({ as: alice })
      //      const res_after = await expr_lit_date_0.expr_lit_date_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_lit_date_1', async () => {
      await expr_lit_date_1.expr_lit_date_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_lit_date_1.expr_lit_date_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_lit_date_1.expr_lit_date_1.exec({ as: alice })
      //      const res_after = await expr_lit_date_1.expr_lit_date_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_lit_date_2', async () => {
      await expr_lit_date_2.expr_lit_date_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_lit_date_2.expr_lit_date_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_lit_date_2.expr_lit_date_2.exec({ as: alice })
      //      const res_after = await expr_lit_date_2.expr_lit_date_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_lit_date_3', async () => {
      await expr_lit_date_3.expr_lit_date_3.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_lit_date_3.expr_lit_date_3.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_lit_date_3.expr_lit_date_3.exec({ as: alice })
      //      const res_after = await expr_lit_date_3.expr_lit_date_3.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_lit_date_4', async () => {
      await expr_lit_date_4.expr_lit_date_4.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_lit_date_4.expr_lit_date_4.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_lit_date_4.expr_lit_date_4.exec({ as: alice })
      //      const res_after = await expr_lit_date_4.expr_lit_date_4.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_lit_dur', async () => {
      await expr_lit_dur.expr_lit_dur.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_lit_dur.expr_lit_dur.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_lit_dur.expr_lit_dur.exec({ as: alice })
      //      const res_after = await expr_lit_dur.expr_lit_dur.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_lit_int', async () => {
      await expr_lit_int.expr_lit_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_lit_int.expr_lit_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_lit_int.expr_lit_int.exec({ as: alice })
      //      const res_after = await expr_lit_int.expr_lit_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_lit_int_neg', async () => {
      await expr_lit_int_neg.expr_lit_int_neg.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_lit_int_neg.expr_lit_int_neg.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_lit_int_neg.expr_lit_int_neg.exec({ as: alice })
      //      const res_after = await expr_lit_int_neg.expr_lit_int_neg.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_lit_nat', async () => {
      await expr_lit_nat.expr_lit_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_lit_nat.expr_lit_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_lit_nat.expr_lit_nat.exec({ as: alice })
      //      const res_after = await expr_lit_nat.expr_lit_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_lit_opt_none', async () => {
      await expr_lit_opt_none.expr_lit_opt_none.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_lit_opt_none.expr_lit_opt_none.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_lit_opt_none.expr_lit_opt_none.exec({ as: alice })
      //      const res_after = await expr_lit_opt_none.expr_lit_opt_none.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_lit_opt_some', async () => {
      await expr_lit_opt_some.expr_lit_opt_some.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_lit_opt_some.expr_lit_opt_some.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_lit_opt_some.expr_lit_opt_some.exec({ as: alice })
      //      const res_after = await expr_lit_opt_some.expr_lit_opt_some.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_lit_or_left', async () => {
      await expr_lit_or_left.expr_lit_or_left.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_lit_or_left.expr_lit_or_left.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_lit_or_left.expr_lit_or_left.exec({ as: alice })
      //      const res_after = await expr_lit_or_left.expr_lit_or_left.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_lit_or_right', async () => {
      await expr_lit_or_right.expr_lit_or_right.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_lit_or_right.expr_lit_or_right.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_lit_or_right.expr_lit_or_right.exec({ as: alice })
      //      const res_after = await expr_lit_or_right.expr_lit_or_right.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_lit_rat_dec', async () => {
      await expr_lit_rat_dec.expr_lit_rat_dec.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_lit_rat_dec.expr_lit_rat_dec.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_lit_rat_dec.expr_lit_rat_dec.exec({ as: alice })
      //      const res_after = await expr_lit_rat_dec.expr_lit_rat_dec.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_lit_rat_dec_neg', async () => {
      await expr_lit_rat_dec_neg.expr_lit_rat_dec_neg.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_lit_rat_dec_neg.expr_lit_rat_dec_neg.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_lit_rat_dec_neg.expr_lit_rat_dec_neg.exec({ as: alice })
      //      const res_after = await expr_lit_rat_dec_neg.expr_lit_rat_dec_neg.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_lit_rat_div', async () => {
      await expr_lit_rat_div.expr_lit_rat_div.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_lit_rat_div.expr_lit_rat_div.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_lit_rat_div.expr_lit_rat_div.exec({ as: alice })
      //      const res_after = await expr_lit_rat_div.expr_lit_rat_div.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_lit_rat_div_neg', async () => {
      await expr_lit_rat_div_neg.expr_lit_rat_div_neg.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_lit_rat_div_neg.expr_lit_rat_div_neg.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_lit_rat_div_neg.expr_lit_rat_div_neg.exec({ as: alice })
      //      const res_after = await expr_lit_rat_div_neg.expr_lit_rat_div_neg.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_lit_str', async () => {
      await expr_lit_str.expr_lit_str.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_lit_str.expr_lit_str.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_lit_str.expr_lit_str.exec({ as: alice })
      //      const res_after = await expr_lit_str.expr_lit_str.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_lit_tuple', async () => {
      await expr_lit_tuple.expr_lit_tuple.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_lit_tuple.expr_lit_tuple.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_lit_tuple.expr_lit_tuple.exec({ as: alice })
      //      const res_after = await expr_lit_tuple.expr_lit_tuple.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_lit_unit', async () => {
      await expr_lit_unit.expr_lit_unit.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_lit_unit.expr_lit_unit.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_lit_unit.expr_lit_unit.exec({ as: alice })
      //      const res_after = await expr_lit_unit.expr_lit_unit.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_make_big_map', async () => {
      await expr_make_big_map.expr_make_big_map.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_make_big_map.expr_make_big_map.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_make_big_map.expr_make_big_map.exec({ as: alice })
      //      const res_after = await expr_make_big_map.expr_make_big_map.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_make_big_map_empty', async () => {
      await expr_make_big_map_empty.expr_make_big_map_empty.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_make_big_map_empty.expr_make_big_map_empty.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_make_big_map_empty.expr_make_big_map_empty.exec({ as: alice })
      //      const res_after = await expr_make_big_map_empty.expr_make_big_map_empty.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_make_list', async () => {
      await expr_make_list.expr_make_list.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_make_list.expr_make_list.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_make_list.expr_make_list.exec({ as: alice })
      //      const res_after = await expr_make_list.expr_make_list.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_make_list_empty', async () => {
      await expr_make_list_empty.expr_make_list_empty.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_make_list_empty.expr_make_list_empty.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_make_list_empty.expr_make_list_empty.exec({ as: alice })
      //      const res_after = await expr_make_list_empty.expr_make_list_empty.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_make_map', async () => {
      await expr_make_map.expr_make_map.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_make_map.expr_make_map.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_make_map.expr_make_map.exec({ as: alice })
      //      const res_after = await expr_make_map.expr_make_map.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_make_map_empty', async () => {
      await expr_make_map_empty.expr_make_map_empty.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_make_map_empty.expr_make_map_empty.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_make_map_empty.expr_make_map_empty.exec({ as: alice })
      //      const res_after = await expr_make_map_empty.expr_make_map_empty.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_make_set', async () => {
      await expr_make_set.expr_make_set.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_make_set.expr_make_set.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_make_set.expr_make_set.exec({ as: alice })
      //      const res_after = await expr_make_set.expr_make_set.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_make_set_empty', async () => {
      await expr_make_set_empty.expr_make_set_empty.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_make_set_empty.expr_make_set_empty.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_make_set_empty.expr_make_set_empty.exec({ as: alice })
      //      const res_after = await expr_make_set_empty.expr_make_set_empty.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_map_contains', async () => {
      await expr_map_contains.expr_map_contains.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_map_contains.expr_map_contains.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_map_contains.expr_map_contains.exec({ as: alice })
      //      const res_after = await expr_map_contains.expr_map_contains.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_map_get', async () => {
      await expr_map_get.expr_map_get.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_map_get.expr_map_get.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_map_get.expr_map_get.exec({ as: alice })
      //      const res_after = await expr_map_get.expr_map_get.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_map_length', async () => {
      await expr_map_length.expr_map_length.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_map_length.expr_map_length.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_map_length.expr_map_length.exec({ as: alice })
      //      const res_after = await expr_map_length.expr_map_length.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_map_lit', async () => {
      await expr_map_lit.expr_map_lit.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_map_lit.expr_map_lit.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_map_lit.expr_map_lit.exec({ as: alice })
      //      const res_after = await expr_map_lit.expr_map_lit.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_map_map', async () => {
      await expr_map_map.expr_map_map.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_map_map.expr_map_map.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_map_map.expr_map_map.exec({ as: alice })
      //      const res_after = await expr_map_map.expr_map_map.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_map_put', async () => {
      await expr_map_put.expr_map_put.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_map_put.expr_map_put.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_map_put.expr_map_put.exec({ as: alice })
      //      const res_after = await expr_map_put.expr_map_put.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_map_remove', async () => {
      await expr_map_remove.expr_map_remove.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_map_remove.expr_map_remove.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_map_remove.expr_map_remove.exec({ as: alice })
      //      const res_after = await expr_map_remove.expr_map_remove.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_map_update', async () => {
      await expr_map_update.expr_map_update.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_map_update.expr_map_update.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_map_update.expr_map_update.exec({ as: alice })
      //      const res_after = await expr_map_update.expr_map_update.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_contains', async () => {
      await expr_method_asset_contains.expr_method_asset_contains.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_contains.expr_method_asset_contains.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_contains.expr_method_asset_contains.exec({ as: alice })
      //      const res_after = await expr_method_asset_contains.expr_method_asset_contains.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_contains_aggregate', async () => {
      await expr_method_asset_contains_aggregate.expr_method_asset_contains_aggregate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_contains_aggregate.expr_method_asset_contains_aggregate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_contains_aggregate.expr_method_asset_contains_aggregate.exec({ as: alice })
      //      const res_after = await expr_method_asset_contains_aggregate.expr_method_asset_contains_aggregate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_contains_one_field', async () => {
      await expr_method_asset_contains_one_field.expr_method_asset_contains_one_field.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_contains_one_field.expr_method_asset_contains_one_field.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_contains_one_field.expr_method_asset_contains_one_field.exec({ as: alice })
      //      const res_after = await expr_method_asset_contains_one_field.expr_method_asset_contains_one_field.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_contains_partition', async () => {
      await expr_method_asset_contains_partition.expr_method_asset_contains_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_contains_partition.expr_method_asset_contains_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_contains_partition.expr_method_asset_contains_partition.exec({ as: alice })
      //      const res_after = await expr_method_asset_contains_partition.expr_method_asset_contains_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_contains_view', async () => {
      await expr_method_asset_contains_view.expr_method_asset_contains_view.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_contains_view.expr_method_asset_contains_view.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_contains_view.expr_method_asset_contains_view.exec({ as: alice })
      //      const res_after = await expr_method_asset_contains_view.expr_method_asset_contains_view.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_count', async () => {
      await expr_method_asset_count.expr_method_asset_count.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_count.expr_method_asset_count.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_count.expr_method_asset_count.exec({ as: alice })
      //      const res_after = await expr_method_asset_count.expr_method_asset_count.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_count_aggregate', async () => {
      await expr_method_asset_count_aggregate.expr_method_asset_count_aggregate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_count_aggregate.expr_method_asset_count_aggregate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_count_aggregate.expr_method_asset_count_aggregate.exec({ as: alice })
      //      const res_after = await expr_method_asset_count_aggregate.expr_method_asset_count_aggregate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_count_one_field', async () => {
      await expr_method_asset_count_one_field.expr_method_asset_count_one_field.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_count_one_field.expr_method_asset_count_one_field.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_count_one_field.expr_method_asset_count_one_field.exec({ as: alice })
      //      const res_after = await expr_method_asset_count_one_field.expr_method_asset_count_one_field.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_count_partition', async () => {
      await expr_method_asset_count_partition.expr_method_asset_count_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_count_partition.expr_method_asset_count_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_count_partition.expr_method_asset_count_partition.exec({ as: alice })
      //      const res_after = await expr_method_asset_count_partition.expr_method_asset_count_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_count_view', async () => {
      await expr_method_asset_count_view.expr_method_asset_count_view.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_count_view.expr_method_asset_count_view.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_count_view.expr_method_asset_count_view.exec({ as: alice })
      //      const res_after = await expr_method_asset_count_view.expr_method_asset_count_view.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_get', async () => {
      await expr_method_asset_get.expr_method_asset_get.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_get.expr_method_asset_get.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_get.expr_method_asset_get.exec({ as: alice })
      //      const res_after = await expr_method_asset_get.expr_method_asset_get.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_head', async () => {
      await expr_method_asset_head.expr_method_asset_head.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_head.expr_method_asset_head.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_head.expr_method_asset_head.exec({ as: alice })
      //      const res_after = await expr_method_asset_head.expr_method_asset_head.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_head_aggregate', async () => {
      await expr_method_asset_head_aggregate.expr_method_asset_head_aggregate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_head_aggregate.expr_method_asset_head_aggregate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_head_aggregate.expr_method_asset_head_aggregate.exec({ as: alice })
      //      const res_after = await expr_method_asset_head_aggregate.expr_method_asset_head_aggregate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_head_one_field', async () => {
      await expr_method_asset_head_one_field.expr_method_asset_head_one_field.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_head_one_field.expr_method_asset_head_one_field.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_head_one_field.expr_method_asset_head_one_field.exec({ as: alice })
      //      const res_after = await expr_method_asset_head_one_field.expr_method_asset_head_one_field.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_head_partition', async () => {
      await expr_method_asset_head_partition.expr_method_asset_head_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_head_partition.expr_method_asset_head_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_head_partition.expr_method_asset_head_partition.exec({ as: alice })
      //      const res_after = await expr_method_asset_head_partition.expr_method_asset_head_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_head_view', async () => {
      await expr_method_asset_head_view.expr_method_asset_head_view.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_head_view.expr_method_asset_head_view.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_head_view.expr_method_asset_head_view.exec({ as: alice })
      //      const res_after = await expr_method_asset_head_view.expr_method_asset_head_view.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_nth', async () => {
      await expr_method_asset_nth.expr_method_asset_nth.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_nth.expr_method_asset_nth.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_nth.expr_method_asset_nth.exec({ as: alice })
      //      const res_after = await expr_method_asset_nth.expr_method_asset_nth.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_nth_aggregate', async () => {
      await expr_method_asset_nth_aggregate.expr_method_asset_nth_aggregate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_nth_aggregate.expr_method_asset_nth_aggregate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_nth_aggregate.expr_method_asset_nth_aggregate.exec({ as: alice })
      //      const res_after = await expr_method_asset_nth_aggregate.expr_method_asset_nth_aggregate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_nth_one_field', async () => {
      await expr_method_asset_nth_one_field.expr_method_asset_nth_one_field.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_nth_one_field.expr_method_asset_nth_one_field.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_nth_one_field.expr_method_asset_nth_one_field.exec({ as: alice })
      //      const res_after = await expr_method_asset_nth_one_field.expr_method_asset_nth_one_field.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_nth_partition', async () => {
      await expr_method_asset_nth_partition.expr_method_asset_nth_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_nth_partition.expr_method_asset_nth_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_nth_partition.expr_method_asset_nth_partition.exec({ as: alice })
      //      const res_after = await expr_method_asset_nth_partition.expr_method_asset_nth_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_nth_view', async () => {
      await expr_method_asset_nth_view.expr_method_asset_nth_view.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_nth_view.expr_method_asset_nth_view.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_nth_view.expr_method_asset_nth_view.exec({ as: alice })
      //      const res_after = await expr_method_asset_nth_view.expr_method_asset_nth_view.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_select', async () => {
      await expr_method_asset_select.expr_method_asset_select.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_select.expr_method_asset_select.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_select.expr_method_asset_select.exec({ as: alice })
      //      const res_after = await expr_method_asset_select.expr_method_asset_select.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_select_aggregate', async () => {
      await expr_method_asset_select_aggregate.expr_method_asset_select_aggregate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_select_aggregate.expr_method_asset_select_aggregate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_select_aggregate.expr_method_asset_select_aggregate.exec({ as: alice })
      //      const res_after = await expr_method_asset_select_aggregate.expr_method_asset_select_aggregate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_select_one_field', async () => {
      await expr_method_asset_select_one_field.expr_method_asset_select_one_field.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_select_one_field.expr_method_asset_select_one_field.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_select_one_field.expr_method_asset_select_one_field.exec({ as: alice })
      //      const res_after = await expr_method_asset_select_one_field.expr_method_asset_select_one_field.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_select_partition', async () => {
      await expr_method_asset_select_partition.expr_method_asset_select_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_select_partition.expr_method_asset_select_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_select_partition.expr_method_asset_select_partition.exec({ as: alice })
      //      const res_after = await expr_method_asset_select_partition.expr_method_asset_select_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_select_view', async () => {
      await expr_method_asset_select_view.expr_method_asset_select_view.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_select_view.expr_method_asset_select_view.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_select_view.expr_method_asset_select_view.exec({ as: alice })
      //      const res_after = await expr_method_asset_select_view.expr_method_asset_select_view.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_sort', async () => {
      await expr_method_asset_sort.expr_method_asset_sort.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_sort.expr_method_asset_sort.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_sort.expr_method_asset_sort.exec({ as: alice })
      //      const res_after = await expr_method_asset_sort.expr_method_asset_sort.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_sort_aggregate', async () => {
      await expr_method_asset_sort_aggregate.expr_method_asset_sort_aggregate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_sort_aggregate.expr_method_asset_sort_aggregate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_sort_aggregate.expr_method_asset_sort_aggregate.exec({ as: alice })
      //      const res_after = await expr_method_asset_sort_aggregate.expr_method_asset_sort_aggregate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_sort_one_field', async () => {
      await expr_method_asset_sort_one_field.expr_method_asset_sort_one_field.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_sort_one_field.expr_method_asset_sort_one_field.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_sort_one_field.expr_method_asset_sort_one_field.exec({ as: alice })
      //      const res_after = await expr_method_asset_sort_one_field.expr_method_asset_sort_one_field.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_sort_partition', async () => {
      await expr_method_asset_sort_partition.expr_method_asset_sort_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_sort_partition.expr_method_asset_sort_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_sort_partition.expr_method_asset_sort_partition.exec({ as: alice })
      //      const res_after = await expr_method_asset_sort_partition.expr_method_asset_sort_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_sort_view', async () => {
      await expr_method_asset_sort_view.expr_method_asset_sort_view.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_sort_view.expr_method_asset_sort_view.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_sort_view.expr_method_asset_sort_view.exec({ as: alice })
      //      const res_after = await expr_method_asset_sort_view.expr_method_asset_sort_view.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_sum', async () => {
      await expr_method_asset_sum.expr_method_asset_sum.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_sum.expr_method_asset_sum.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_sum.expr_method_asset_sum.exec({ as: alice })
      //      const res_after = await expr_method_asset_sum.expr_method_asset_sum.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_sum_aggregate', async () => {
      await expr_method_asset_sum_aggregate.expr_method_asset_sum_aggregate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_sum_aggregate.expr_method_asset_sum_aggregate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_sum_aggregate.expr_method_asset_sum_aggregate.exec({ as: alice })
      //      const res_after = await expr_method_asset_sum_aggregate.expr_method_asset_sum_aggregate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_sum_one_field', async () => {
      await expr_method_asset_sum_one_field.expr_method_asset_sum_one_field.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_sum_one_field.expr_method_asset_sum_one_field.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_sum_one_field.expr_method_asset_sum_one_field.exec({ as: alice })
      //      const res_after = await expr_method_asset_sum_one_field.expr_method_asset_sum_one_field.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_sum_partition', async () => {
      await expr_method_asset_sum_partition.expr_method_asset_sum_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_sum_partition.expr_method_asset_sum_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_sum_partition.expr_method_asset_sum_partition.exec({ as: alice })
      //      const res_after = await expr_method_asset_sum_partition.expr_method_asset_sum_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_sum_rational', async () => {
      await expr_method_asset_sum_rational.expr_method_asset_sum_rational.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_sum_rational.expr_method_asset_sum_rational.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_sum_rational.expr_method_asset_sum_rational.exec({ as: alice })
      //      const res_after = await expr_method_asset_sum_rational.expr_method_asset_sum_rational.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_sum_view', async () => {
      await expr_method_asset_sum_view.expr_method_asset_sum_view.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_sum_view.expr_method_asset_sum_view.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_sum_view.expr_method_asset_sum_view.exec({ as: alice })
      //      const res_after = await expr_method_asset_sum_view.expr_method_asset_sum_view.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_tail', async () => {
      await expr_method_asset_tail.expr_method_asset_tail.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_tail.expr_method_asset_tail.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_tail.expr_method_asset_tail.exec({ as: alice })
      //      const res_after = await expr_method_asset_tail.expr_method_asset_tail.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_tail_aggregate', async () => {
      await expr_method_asset_tail_aggregate.expr_method_asset_tail_aggregate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_tail_aggregate.expr_method_asset_tail_aggregate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_tail_aggregate.expr_method_asset_tail_aggregate.exec({ as: alice })
      //      const res_after = await expr_method_asset_tail_aggregate.expr_method_asset_tail_aggregate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_tail_one_field', async () => {
      await expr_method_asset_tail_one_field.expr_method_asset_tail_one_field.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_tail_one_field.expr_method_asset_tail_one_field.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_tail_one_field.expr_method_asset_tail_one_field.exec({ as: alice })
      //      const res_after = await expr_method_asset_tail_one_field.expr_method_asset_tail_one_field.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_tail_partition', async () => {
      await expr_method_asset_tail_partition.expr_method_asset_tail_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_tail_partition.expr_method_asset_tail_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_tail_partition.expr_method_asset_tail_partition.exec({ as: alice })
      //      const res_after = await expr_method_asset_tail_partition.expr_method_asset_tail_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_method_asset_tail_view', async () => {
      await expr_method_asset_tail_view.expr_method_asset_tail_view.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_method_asset_tail_view.expr_method_asset_tail_view.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_method_asset_tail_view.expr_method_asset_tail_view.exec({ as: alice })
      //      const res_after = await expr_method_asset_tail_view.expr_method_asset_tail_view.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_multicmp', async () => {
      await expr_multicmp.expr_multicmp.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_multicmp.expr_multicmp.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_multicmp.expr_multicmp.exec({ as: alice })
      //      const res_after = await expr_multicmp.expr_multicmp.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_option_map', async () => {
      await expr_option_map.expr_option_map.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_option_map.expr_option_map.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_option_map.expr_option_map.exec({ as: alice })
      //      const res_after = await expr_option_map.expr_option_map.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_record_lit', async () => {
      await expr_record_lit.expr_record_lit.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_record_lit.expr_record_lit.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_record_lit.expr_record_lit.exec({ as: alice })
      //      const res_after = await expr_record_lit.expr_record_lit.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_record_update_asset_in_formula', async () => {
      await expr_record_update_asset_in_formula.expr_record_update_asset_in_formula.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_record_update_asset_in_formula.expr_record_update_asset_in_formula.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_record_update_asset_in_formula.expr_record_update_asset_in_formula.exec({ as: alice })
      //      const res_after = await expr_record_update_asset_in_formula.expr_record_update_asset_in_formula.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_record_update_record_in_exec', async () => {
      await expr_record_update_record_in_exec.expr_record_update_record_in_exec.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_record_update_record_in_exec.expr_record_update_record_in_exec.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_record_update_record_in_exec.expr_record_update_record_in_exec.exec({ as: alice })
      //      const res_after = await expr_record_update_record_in_exec.expr_record_update_record_in_exec.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_record_update_record_in_formula', async () => {
      await expr_record_update_record_in_formula.expr_record_update_record_in_formula.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_record_update_record_in_formula.expr_record_update_record_in_formula.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_record_update_record_in_formula.expr_record_update_record_in_formula.exec({ as: alice })
      //      const res_after = await expr_record_update_record_in_formula.expr_record_update_record_in_formula.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_set_add', async () => {
      await expr_set_add.expr_set_add.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_set_add.expr_set_add.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_set_add.expr_set_add.exec({ as: alice })
      //      const res_after = await expr_set_add.expr_set_add.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_set_contains', async () => {
      await expr_set_contains.expr_set_contains.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_set_contains.expr_set_contains.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_set_contains.expr_set_contains.exec({ as: alice })
      //      const res_after = await expr_set_contains.expr_set_contains.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_set_length', async () => {
      await expr_set_length.expr_set_length.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_set_length.expr_set_length.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_set_length.expr_set_length.exec({ as: alice })
      //      const res_after = await expr_set_length.expr_set_length.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_set_lit', async () => {
      await expr_set_lit.expr_set_lit.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_set_lit.expr_set_lit.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_set_lit.expr_set_lit.exec({ as: alice })
      //      const res_after = await expr_set_lit.expr_set_lit.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_set_remove', async () => {
      await expr_set_remove.expr_set_remove.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_set_remove.expr_set_remove.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_set_remove.expr_set_remove.exec({ as: alice })
      //      const res_after = await expr_set_remove.expr_set_remove.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_set_update', async () => {
      await expr_set_update.expr_set_update.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_set_update.expr_set_update.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_set_update.expr_set_update.exec({ as: alice })
      //      const res_after = await expr_set_update.expr_set_update.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_tuple_access', async () => {
      await expr_tuple_access.expr_tuple_access.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_tuple_access.expr_tuple_access.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_tuple_access.expr_tuple_access.exec({ as: alice })
      //      const res_after = await expr_tuple_access.expr_tuple_access.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('expr_tuple_access_simple', async () => {
      await expr_tuple_access_simple.expr_tuple_access_simple.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await expr_tuple_access_simple.expr_tuple_access_simple.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await expr_tuple_access_simple.expr_tuple_access_simple.exec({ as: alice })
      //      const res_after = await expr_tuple_access_simple.expr_tuple_access_simple.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('fa12_false', async () => {
      await fa12_false.fa12_false.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await fa12_false.fa12_false.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await fa12_false.fa12_false.exec({ as: alice })
      //      const res_after = await fa12_false.fa12_false.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('fa12_simple', async () => {
      await fa12_simple.fa12_simple.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await fa12_simple.fa12_simple.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await fa12_simple.fa12_simple.exec({ as: alice })
      //      const res_after = await fa12_simple.fa12_simple.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('fail_', async () => {
      await fail_.fail_.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await fail_.fail_.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await fail_.fail_.exec({ as: alice })
      //      const res_after = await fail_.fail_.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('fail_for', async () => {
      await fail_for.fail_for.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await fail_for.fail_for.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await fail_for.fail_for.exec({ as: alice })
      //      const res_after = await fail_for.fail_for.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('fail_if', async () => {
      await fail_if.fail_if.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await fail_if.fail_if.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await fail_if.fail_if.exec({ as: alice })
      //      const res_after = await fail_if.fail_if.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('fail_match_list', async () => {
      await fail_match_list.fail_match_list.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await fail_match_list.fail_match_list.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await fail_match_list.fail_match_list.exec({ as: alice })
      //      const res_after = await fail_match_list.fail_match_list.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('fail_match_option', async () => {
      await fail_match_option.fail_match_option.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await fail_match_option.fail_match_option.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await fail_match_option.fail_match_option.exec({ as: alice })
      //      const res_after = await fail_match_option.fail_match_option.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('fail_while', async () => {
      await fail_while.fail_while.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await fail_while.fail_while.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await fail_while.fail_while.exec({ as: alice })
      //      const res_after = await fail_while.fail_while.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('fold_reverse', async () => {
      await fold_reverse.fold_reverse.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await fold_reverse.fold_reverse.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await fold_reverse.fold_reverse.exec({ as: alice })
      //      const res_after = await fold_reverse.fold_reverse.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('formula_added_asset', async () => {
      await formula_added_asset.formula_added_asset.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await formula_added_asset.formula_added_asset.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await formula_added_asset.formula_added_asset.exec({ as: alice })
      //      const res_after = await formula_added_asset.formula_added_asset.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('fun', async () => {
      await fun.fun.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await fun.fun.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await fun.fun.exec({ as: alice })
      //      const res_after = await fun.fun.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('get_in_require_failif', async () => {
      await get_in_require_failif.get_in_require_failif.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await get_in_require_failif.get_in_require_failif.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await get_in_require_failif.get_in_require_failif.exec({ as: alice })
      //      const res_after = await get_in_require_failif.get_in_require_failif.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('get_some_with_msg', async () => {
      await get_some_with_msg.get_some_with_msg.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await get_some_with_msg.get_some_with_msg.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await get_some_with_msg.get_some_with_msg.exec({ as: alice })
      //      const res_after = await get_some_with_msg.get_some_with_msg.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('greedy_and', async () => {
      await greedy_and.greedy_and.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await greedy_and.greedy_and.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await greedy_and.greedy_and.exec({ as: alice })
      //      const res_after = await greedy_and.greedy_and.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('greedy_or', async () => {
      await greedy_or.greedy_or.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await greedy_or.greedy_or.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await greedy_or.greedy_or.exec({ as: alice })
      //      const res_after = await greedy_or.greedy_or.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('implicit_cast_to_view', async () => {
      await implicit_cast_to_view.implicit_cast_to_view.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await implicit_cast_to_view.implicit_cast_to_view.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await implicit_cast_to_view.implicit_cast_to_view.exec({ as: alice })
      //      const res_after = await implicit_cast_to_view.implicit_cast_to_view.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('implicit_the', async () => {
      await implicit_the.implicit_the.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await implicit_the.implicit_the.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await implicit_the.implicit_the.exec({ as: alice })
      //      const res_after = await implicit_the.implicit_the.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('init_lambda', async () => {
      await init_lambda.init_lambda.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await init_lambda.init_lambda.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await init_lambda.init_lambda.exec({ as: alice })
      //      const res_after = await init_lambda.init_lambda.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('instr_list_prepend', async () => {
      await instr_list_prepend.instr_list_prepend.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await instr_list_prepend.instr_list_prepend.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await instr_list_prepend.instr_list_prepend.exec({ as: alice })
      //      const res_after = await instr_list_prepend.instr_list_prepend.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('instr_map_put', async () => {
      await instr_map_put.instr_map_put.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await instr_map_put.instr_map_put.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await instr_map_put.instr_map_put.exec({ as: alice })
      //      const res_after = await instr_map_put.instr_map_put.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('instr_map_remove', async () => {
      await instr_map_remove.instr_map_remove.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await instr_map_remove.instr_map_remove.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await instr_map_remove.instr_map_remove.exec({ as: alice })
      //      const res_after = await instr_map_remove.instr_map_remove.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('instr_map_update_local_record', async () => {
      await instr_map_update_local_record.instr_map_update_local_record.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await instr_map_update_local_record.instr_map_update_local_record.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await instr_map_update_local_record.instr_map_update_local_record.exec({ as: alice })
      //      const res_after = await instr_map_update_local_record.instr_map_update_local_record.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('instr_map_update_local_var', async () => {
      await instr_map_update_local_var.instr_map_update_local_var.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await instr_map_update_local_var.instr_map_update_local_var.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await instr_map_update_local_var.instr_map_update_local_var.exec({ as: alice })
      //      const res_after = await instr_map_update_local_var.instr_map_update_local_var.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('instr_map_update_storage_record', async () => {
      await instr_map_update_storage_record.instr_map_update_storage_record.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await instr_map_update_storage_record.instr_map_update_storage_record.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await instr_map_update_storage_record.instr_map_update_storage_record.exec({ as: alice })
      //      const res_after = await instr_map_update_storage_record.instr_map_update_storage_record.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('instr_map_update_storage_var', async () => {
      await instr_map_update_storage_var.instr_map_update_storage_var.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await instr_map_update_storage_var.instr_map_update_storage_var.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await instr_map_update_storage_var.instr_map_update_storage_var.exec({ as: alice })
      //      const res_after = await instr_map_update_storage_var.instr_map_update_storage_var.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('instr_set_add', async () => {
      await instr_set_add.instr_set_add.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await instr_set_add.instr_set_add.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await instr_set_add.instr_set_add.exec({ as: alice })
      //      const res_after = await instr_set_add.instr_set_add.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('instr_set_remove', async () => {
      await instr_set_remove.instr_set_remove.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await instr_set_remove.instr_set_remove.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await instr_set_remove.instr_set_remove.exec({ as: alice })
      //      const res_after = await instr_set_remove.instr_set_remove.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('instr_set_update_add', async () => {
      await instr_set_update_add.instr_set_update_add.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await instr_set_update_add.instr_set_update_add.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await instr_set_update_add.instr_set_update_add.exec({ as: alice })
      //      const res_after = await instr_set_update_add.instr_set_update_add.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('instr_set_update_remove', async () => {
      await instr_set_update_remove.instr_set_update_remove.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await instr_set_update_remove.instr_set_update_remove.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await instr_set_update_remove.instr_set_update_remove.exec({ as: alice })
      //      const res_after = await instr_set_update_remove.instr_set_update_remove.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('int_to_date', async () => {
      await int_to_date.int_to_date.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await int_to_date.int_to_date.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await int_to_date.int_to_date.exec({ as: alice })
      //      const res_after = await int_to_date.int_to_date.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('invariants_on_states', async () => {
      await invariants_on_states.invariants_on_states.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await invariants_on_states.invariants_on_states.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await invariants_on_states.invariants_on_states.exec({ as: alice })
      //      const res_after = await invariants_on_states.invariants_on_states.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('invariants_on_variable', async () => {
      await invariants_on_variable.invariants_on_variable.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await invariants_on_variable.invariants_on_variable.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await invariants_on_variable.invariants_on_variable.exec({ as: alice })
      //      const res_after = await invariants_on_variable.invariants_on_variable.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('iterable_big_map_assign', async () => {
      await iterable_big_map_assign.iterable_big_map_assign.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await iterable_big_map_assign.iterable_big_map_assign.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await iterable_big_map_assign.iterable_big_map_assign.exec({ as: alice })
      //      const res_after = await iterable_big_map_assign.iterable_big_map_assign.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('iterable_big_map_contains', async () => {
      await iterable_big_map_contains.iterable_big_map_contains.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await iterable_big_map_contains.iterable_big_map_contains.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await iterable_big_map_contains.iterable_big_map_contains.exec({ as: alice })
      //      const res_after = await iterable_big_map_contains.iterable_big_map_contains.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('iterable_big_map_for', async () => {
      await iterable_big_map_for.iterable_big_map_for.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await iterable_big_map_for.iterable_big_map_for.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await iterable_big_map_for.iterable_big_map_for.exec({ as: alice })
      //      const res_after = await iterable_big_map_for.iterable_big_map_for.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('iterable_big_map_get', async () => {
      await iterable_big_map_get.iterable_big_map_get.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await iterable_big_map_get.iterable_big_map_get.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await iterable_big_map_get.iterable_big_map_get.exec({ as: alice })
      //      const res_after = await iterable_big_map_get.iterable_big_map_get.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('iterable_big_map_length', async () => {
      await iterable_big_map_length.iterable_big_map_length.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await iterable_big_map_length.iterable_big_map_length.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await iterable_big_map_length.iterable_big_map_length.exec({ as: alice })
      //      const res_after = await iterable_big_map_length.iterable_big_map_length.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('iterable_big_map_put', async () => {
      await iterable_big_map_put.iterable_big_map_put.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await iterable_big_map_put.iterable_big_map_put.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await iterable_big_map_put.iterable_big_map_put.exec({ as: alice })
      //      const res_after = await iterable_big_map_put.iterable_big_map_put.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('iterable_big_map_remove', async () => {
      await iterable_big_map_remove.iterable_big_map_remove.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await iterable_big_map_remove.iterable_big_map_remove.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await iterable_big_map_remove.iterable_big_map_remove.exec({ as: alice })
      //      const res_after = await iterable_big_map_remove.iterable_big_map_remove.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('iterable_big_map_storage_decl', async () => {
      await iterable_big_map_storage_decl.iterable_big_map_storage_decl.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await iterable_big_map_storage_decl.iterable_big_map_storage_decl.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await iterable_big_map_storage_decl.iterable_big_map_storage_decl.exec({ as: alice })
      //      const res_after = await iterable_big_map_storage_decl.iterable_big_map_storage_decl.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('iterable_big_map_test', async () => {
      await iterable_big_map_test.iterable_big_map_test.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await iterable_big_map_test.iterable_big_map_test.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await iterable_big_map_test.iterable_big_map_test.exec({ as: alice })
      //      const res_after = await iterable_big_map_test.iterable_big_map_test.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('key_to_address', async () => {
      await key_to_address.key_to_address.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await key_to_address.key_to_address.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await key_to_address.key_to_address.exec({ as: alice })
      //      const res_after = await key_to_address.key_to_address.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('lang_arith', async () => {
      await lang_arith.lang_arith.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await lang_arith.lang_arith.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await lang_arith.lang_arith.exec({ as: alice })
      //      const res_after = await lang_arith.lang_arith.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('lang_asset', async () => {
      await lang_asset.lang_asset.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await lang_asset.lang_asset.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await lang_asset.lang_asset.exec({ as: alice })
      //      const res_after = await lang_asset.lang_asset.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('lang_assign', async () => {
      await lang_assign.lang_assign.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await lang_assign.lang_assign.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await lang_assign.lang_assign.exec({ as: alice })
      //      const res_after = await lang_assign.lang_assign.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('lang_big_map', async () => {
      await lang_big_map.lang_big_map.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await lang_big_map.lang_big_map.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await lang_big_map.lang_big_map.exec({ as: alice })
      //      const res_after = await lang_big_map.lang_big_map.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('lang_cast', async () => {
      await lang_cast.lang_cast.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await lang_cast.lang_cast.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await lang_cast.lang_cast.exec({ as: alice })
      //      const res_after = await lang_cast.lang_cast.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('lang_cmp', async () => {
      await lang_cmp.lang_cmp.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await lang_cmp.lang_cmp.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await lang_cmp.lang_cmp.exec({ as: alice })
      //      const res_after = await lang_cmp.lang_cmp.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('lang_contract', async () => {
      await lang_contract.lang_contract.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await lang_contract.lang_contract.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await lang_contract.lang_contract.exec({ as: alice })
      //      const res_after = await lang_contract.lang_contract.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('lang_crypto', async () => {
      await lang_crypto.lang_crypto.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await lang_crypto.lang_crypto.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await lang_crypto.lang_crypto.exec({ as: alice })
      //      const res_after = await lang_crypto.lang_crypto.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('lang_cst', async () => {
      await lang_cst.lang_cst.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await lang_cst.lang_cst.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await lang_cst.lang_cst.exec({ as: alice })
      //      const res_after = await lang_cst.lang_cst.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('lang_entry', async () => {
      await lang_entry.lang_entry.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await lang_entry.lang_entry.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await lang_entry.lang_entry.exec({ as: alice })
      //      const res_after = await lang_entry.lang_entry.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('lang_formula_asset_api', async () => {
      await lang_formula_asset_api.lang_formula_asset_api.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await lang_formula_asset_api.lang_formula_asset_api.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await lang_formula_asset_api.lang_formula_asset_api.exec({ as: alice })
      //      const res_after = await lang_formula_asset_api.lang_formula_asset_api.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('lang_funs', async () => {
      await lang_funs.lang_funs.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await lang_funs.lang_funs.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await lang_funs.lang_funs.exec({ as: alice })
      //      const res_after = await lang_funs.lang_funs.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('lang_list', async () => {
      await lang_list.lang_list.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await lang_list.lang_list.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await lang_list.lang_list.exec({ as: alice })
      //      const res_after = await lang_list.lang_list.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('lang_literals', async () => {
      await lang_literals.lang_literals.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await lang_literals.lang_literals.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await lang_literals.lang_literals.exec({ as: alice })
      //      const res_after = await lang_literals.lang_literals.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('lang_map', async () => {
      await lang_map.lang_map.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await lang_map.lang_map.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await lang_map.lang_map.exec({ as: alice })
      //      const res_after = await lang_map.lang_map.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('lang_methods_asset', async () => {
      await lang_methods_asset.lang_methods_asset.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await lang_methods_asset.lang_methods_asset.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await lang_methods_asset.lang_methods_asset.exec({ as: alice })
      //      const res_after = await lang_methods_asset.lang_methods_asset.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('lang_security', async () => {
      await lang_security.lang_security.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await lang_security.lang_security.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await lang_security.lang_security.exec({ as: alice })
      //      const res_after = await lang_security.lang_security.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('lang_set', async () => {
      await lang_set.lang_set.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await lang_set.lang_set.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await lang_set.lang_set.exec({ as: alice })
      //      const res_after = await lang_set.lang_set.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('list_list', async () => {
      await list_list.list_list.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await list_list.list_list.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await list_list.list_list.exec({ as: alice })
      //      const res_after = await list_list.list_list.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('list_nth_out_of_bound', async () => {
      await list_nth_out_of_bound.list_nth_out_of_bound.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await list_nth_out_of_bound.list_nth_out_of_bound.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await list_nth_out_of_bound.list_nth_out_of_bound.exec({ as: alice })
      //      const res_after = await list_nth_out_of_bound.list_nth_out_of_bound.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('list_option', async () => {
      await list_option.list_option.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await list_option.list_option.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await list_option.list_option.exec({ as: alice })
      //      const res_after = await list_option.list_option.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('list_or', async () => {
      await list_or.list_or.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await list_or.list_or.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await list_or.list_or.exec({ as: alice })
      //      const res_after = await list_or.list_or.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('lit_tez_underscore', async () => {
      await lit_tez_underscore.lit_tez_underscore.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await lit_tez_underscore.lit_tez_underscore.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await lit_tez_underscore.lit_tez_underscore.exec({ as: alice })
      //      const res_after = await lit_tez_underscore.lit_tez_underscore.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('literal_in_argument', async () => {
      await literal_in_argument.literal_in_argument.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await literal_in_argument.literal_in_argument.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await literal_in_argument.literal_in_argument.exec({ as: alice })
      //      const res_after = await literal_in_argument.literal_in_argument.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('map_asset', async () => {
      await map_asset.map_asset.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await map_asset.map_asset.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await map_asset.map_asset.exec({ as: alice })
      //      const res_after = await map_asset.map_asset.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('match_entrypoint', async () => {
      await match_entrypoint.match_entrypoint.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await match_entrypoint.match_entrypoint.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await match_entrypoint.match_entrypoint.exec({ as: alice })
      //      const res_after = await match_entrypoint.match_entrypoint.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('max_tez', async () => {
      await max_tez.max_tez.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await max_tez.max_tez.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await max_tez.max_tez.exec({ as: alice })
      //      const res_after = await max_tez.max_tez.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('method_in_dorequire_or_dofailif', async () => {
      await method_in_dorequire_or_dofailif.method_in_dorequire_or_dofailif.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await method_in_dorequire_or_dofailif.method_in_dorequire_or_dofailif.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await method_in_dorequire_or_dofailif.method_in_dorequire_or_dofailif.exec({ as: alice })
      //      const res_after = await method_in_dorequire_or_dofailif.method_in_dorequire_or_dofailif.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('miles_with_expiration_spec', async () => {
      await miles_with_expiration_spec.miles_with_expiration_spec.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await miles_with_expiration_spec.miles_with_expiration_spec.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await miles_with_expiration_spec.miles_with_expiration_spec.exec({ as: alice })
      //      const res_after = await miles_with_expiration_spec.miles_with_expiration_spec.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('multi_e', async () => {
      await multi_e.multi_e.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await multi_e.multi_e.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await multi_e.multi_e.exec({ as: alice })
      //      const res_after = await multi_e.multi_e.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('multi_p', async () => {
      await multi_p.multi_p.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await multi_p.multi_p.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await multi_p.multi_p.exec({ as: alice })
      //      const res_after = await multi_p.multi_p.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('multi_sort', async () => {
      await multi_sort.multi_sort.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await multi_sort.multi_sort.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await multi_sort.multi_sort.exec({ as: alice })
      //      const res_after = await multi_sort.multi_sort.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('multi_update', async () => {
      await multi_update.multi_update.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await multi_update.multi_update.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await multi_update.multi_update.exec({ as: alice })
      //      const res_after = await multi_update.multi_update.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('multi_var_storage', async () => {
      await multi_var_storage.multi_var_storage.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await multi_var_storage.multi_var_storage.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await multi_var_storage.multi_var_storage.exec({ as: alice })
      //      const res_after = await multi_var_storage.multi_var_storage.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('multivars', async () => {
      await multivars.multivars.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await multivars.multivars.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await multivars.multivars.exec({ as: alice })
      //      const res_after = await multivars.multivars.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('multivars1', async () => {
      await multivars1.multivars1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await multivars1.multivars1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await multivars1.multivars1.exec({ as: alice })
      //      const res_after = await multivars1.multivars1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('multivars_simple', async () => {
      await multivars_simple.multivars_simple.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await multivars_simple.multivars_simple.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await multivars_simple.multivars_simple.exec({ as: alice })
      //      const res_after = await multivars_simple.multivars_simple.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('mutez_to_nat', async () => {
      await mutez_to_nat.mutez_to_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await mutez_to_nat.mutez_to_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await mutez_to_nat.mutez_to_nat.exec({ as: alice })
      //      const res_after = await mutez_to_nat.mutez_to_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('nat_to_string', async () => {
      await nat_to_string.nat_to_string.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await nat_to_string.nat_to_string.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await nat_to_string.nat_to_string.exec({ as: alice })
      //      const res_after = await nat_to_string.nat_to_string.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('nested_for', async () => {
      await nested_for.nested_for.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await nested_for.nested_for.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await nested_for.nested_for.exec({ as: alice })
      //      const res_after = await nested_for.nested_for.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('nested_if_return', async () => {
      await nested_if_return.nested_if_return.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await nested_if_return.nested_if_return.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await nested_if_return.nested_if_return.exec({ as: alice })
      //      const res_after = await nested_if_return.nested_if_return.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('no_entrypoint', async () => {
      await no_entrypoint.no_entrypoint.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await no_entrypoint.no_entrypoint.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await no_entrypoint.no_entrypoint.exec({ as: alice })
      //      const res_after = await no_entrypoint.no_entrypoint.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('not_int', async () => {
      await not_int.not_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await not_int.not_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await not_int.not_int.exec({ as: alice })
      //      const res_after = await not_int.not_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('not_nat', async () => {
      await not_nat.not_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await not_nat.not_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await not_nat.not_nat.exec({ as: alice })
      //      const res_after = await not_nat.not_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('nothing', async () => {
      await nothing.nothing.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await nothing.nothing.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await nothing.nothing.exec({ as: alice })
      //      const res_after = await nothing.nothing.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('one_constant', async () => {
      await one_constant.one_constant.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await one_constant.one_constant.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await one_constant.one_constant.exec({ as: alice })
      //      const res_after = await one_constant.one_constant.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('op_assign_rat_update_asset', async () => {
      await op_assign_rat_update_asset.op_assign_rat_update_asset.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await op_assign_rat_update_asset.op_assign_rat_update_asset.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await op_assign_rat_update_asset.op_assign_rat_update_asset.exec({ as: alice })
      //      const res_after = await op_assign_rat_update_asset.op_assign_rat_update_asset.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('parameter_expr_map', async () => {
      await parameter_expr_map.parameter_expr_map.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await parameter_expr_map.parameter_expr_map.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await parameter_expr_map.parameter_expr_map.exec({ as: alice })
      //      const res_after = await parameter_expr_map.parameter_expr_map.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('partial_record', async () => {
      await partial_record.partial_record.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await partial_record.partial_record.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await partial_record.partial_record.exec({ as: alice })
      //      const res_after = await partial_record.partial_record.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('rat_arith_div', async () => {
      await rat_arith_div.rat_arith_div.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await rat_arith_div.rat_arith_div.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await rat_arith_div.rat_arith_div.exec({ as: alice })
      //      const res_after = await rat_arith_div.rat_arith_div.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('rat_arith_minus', async () => {
      await rat_arith_minus.rat_arith_minus.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await rat_arith_minus.rat_arith_minus.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await rat_arith_minus.rat_arith_minus.exec({ as: alice })
      //      const res_after = await rat_arith_minus.rat_arith_minus.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('rat_arith_mult', async () => {
      await rat_arith_mult.rat_arith_mult.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await rat_arith_mult.rat_arith_mult.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await rat_arith_mult.rat_arith_mult.exec({ as: alice })
      //      const res_after = await rat_arith_mult.rat_arith_mult.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('rat_arith_plus', async () => {
      await rat_arith_plus.rat_arith_plus.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await rat_arith_plus.rat_arith_plus.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await rat_arith_plus.rat_arith_plus.exec({ as: alice })
      //      const res_after = await rat_arith_plus.rat_arith_plus.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('rat_arith_uminus', async () => {
      await rat_arith_uminus.rat_arith_uminus.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await rat_arith_uminus.rat_arith_uminus.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await rat_arith_uminus.rat_arith_uminus.exec({ as: alice })
      //      const res_after = await rat_arith_uminus.rat_arith_uminus.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('rat_cmp_eq', async () => {
      await rat_cmp_eq.rat_cmp_eq.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await rat_cmp_eq.rat_cmp_eq.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await rat_cmp_eq.rat_cmp_eq.exec({ as: alice })
      //      const res_after = await rat_cmp_eq.rat_cmp_eq.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('rat_cmp_ge', async () => {
      await rat_cmp_ge.rat_cmp_ge.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await rat_cmp_ge.rat_cmp_ge.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await rat_cmp_ge.rat_cmp_ge.exec({ as: alice })
      //      const res_after = await rat_cmp_ge.rat_cmp_ge.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('rat_cmp_gt', async () => {
      await rat_cmp_gt.rat_cmp_gt.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await rat_cmp_gt.rat_cmp_gt.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await rat_cmp_gt.rat_cmp_gt.exec({ as: alice })
      //      const res_after = await rat_cmp_gt.rat_cmp_gt.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('rat_cmp_le', async () => {
      await rat_cmp_le.rat_cmp_le.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await rat_cmp_le.rat_cmp_le.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await rat_cmp_le.rat_cmp_le.exec({ as: alice })
      //      const res_after = await rat_cmp_le.rat_cmp_le.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('rat_cmp_lt', async () => {
      await rat_cmp_lt.rat_cmp_lt.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await rat_cmp_lt.rat_cmp_lt.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await rat_cmp_lt.rat_cmp_lt.exec({ as: alice })
      //      const res_after = await rat_cmp_lt.rat_cmp_lt.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('rat_dur', async () => {
      await rat_dur.rat_dur.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await rat_dur.rat_dur.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await rat_dur.rat_dur.exec({ as: alice })
      //      const res_after = await rat_dur.rat_dur.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('rat_int', async () => {
      await rat_int.rat_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await rat_int.rat_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await rat_int.rat_int.exec({ as: alice })
      //      const res_after = await rat_int.rat_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('rat_max', async () => {
      await rat_max.rat_max.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await rat_max.rat_max.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await rat_max.rat_max.exec({ as: alice })
      //      const res_after = await rat_max.rat_max.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('rat_min', async () => {
      await rat_min.rat_min.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await rat_min.rat_min.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await rat_min.rat_min.exec({ as: alice })
      //      const res_after = await rat_min.rat_min.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('rat_nat', async () => {
      await rat_nat.rat_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await rat_nat.rat_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await rat_nat.rat_nat.exec({ as: alice })
      //      const res_after = await rat_nat.rat_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('rat_neg', async () => {
      await rat_neg.rat_neg.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await rat_neg.rat_neg.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await rat_neg.rat_neg.exec({ as: alice })
      //      const res_after = await rat_neg.rat_neg.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('rat_tez', async () => {
      await rat_tez.rat_tez.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await rat_tez.rat_tez.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await rat_tez.rat_tez.exec({ as: alice })
      //      const res_after = await rat_tez.rat_tez.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('rat_tez_big', async () => {
      await rat_tez_big.rat_tez_big.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await rat_tez_big.rat_tez_big.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await rat_tez_big.rat_tez_big.exec({ as: alice })
      //      const res_after = await rat_tez_big.rat_tez_big.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('rational_cmp', async () => {
      await rational_cmp.rational_cmp.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await rational_cmp.rational_cmp.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await rational_cmp.rational_cmp.exec({ as: alice })
      //      const res_after = await rational_cmp.rational_cmp.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('rational_duration', async () => {
      await rational_duration.rational_duration.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await rational_duration.rational_duration.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await rational_duration.rational_duration.exec({ as: alice })
      //      const res_after = await rational_duration.rational_duration.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('rational_full', async () => {
      await rational_full.rational_full.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await rational_full.rational_full.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await rational_full.rational_full.exec({ as: alice })
      //      const res_after = await rational_full.rational_full.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('rational_in_formula', async () => {
      await rational_in_formula.rational_in_formula.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await rational_in_formula.rational_in_formula.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await rational_in_formula.rational_in_formula.exec({ as: alice })
      //      const res_after = await rational_in_formula.rational_in_formula.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('rational_rat_tez_mult', async () => {
      await rational_rat_tez_mult.rational_rat_tez_mult.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await rational_rat_tez_mult.rational_rat_tez_mult.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await rational_rat_tez_mult.rational_rat_tez_mult.exec({ as: alice })
      //      const res_after = await rational_rat_tez_mult.rational_rat_tez_mult.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('rational_simple', async () => {
      await rational_simple.rational_simple.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await rational_simple.rational_simple.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await rational_simple.rational_simple.exec({ as: alice })
      //      const res_after = await rational_simple.rational_simple.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('rational_tez_rat_mult', async () => {
      await rational_tez_rat_mult.rational_tez_rat_mult.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await rational_tez_rat_mult.rational_tez_rat_mult.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await rational_tez_rat_mult.rational_tez_rat_mult.exec({ as: alice })
      //      const res_after = await rational_tez_rat_mult.rational_tez_rat_mult.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('rec_update', async () => {
      await rec_update.rec_update.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await rec_update.rec_update.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await rec_update.rec_update.exec({ as: alice })
      //      const res_after = await rec_update.rec_update.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('rec_update2', async () => {
      await rec_update2.rec_update2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await rec_update2.rec_update2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await rec_update2.rec_update2.exec({ as: alice })
      //      const res_after = await rec_update2.rec_update2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('record_container', async () => {
      await record_container.record_container.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await record_container.record_container.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await record_container.record_container.exec({ as: alice })
      //      const res_after = await record_container.record_container.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('record_double_key', async () => {
      await record_double_key.record_double_key.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await record_double_key.record_double_key.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await record_double_key.record_double_key.exec({ as: alice })
      //      const res_after = await record_double_key.record_double_key.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('record_in_enum', async () => {
      await record_in_enum.record_in_enum.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await record_in_enum.record_in_enum.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await record_in_enum.record_in_enum.exec({ as: alice })
      //      const res_after = await record_in_enum.record_in_enum.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('record_update', async () => {
      await record_update.record_update.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await record_update.record_update.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await record_update.record_update.exec({ as: alice })
      //      const res_after = await record_update.record_update.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('remove_asset_with_partition', async () => {
      await remove_asset_with_partition.remove_asset_with_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await remove_asset_with_partition.remove_asset_with_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await remove_asset_with_partition.remove_asset_with_partition.exec({ as: alice })
      //      const res_after = await remove_asset_with_partition.remove_asset_with_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('reverse_otherwise', async () => {
      await reverse_otherwise.reverse_otherwise.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await reverse_otherwise.reverse_otherwise.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await reverse_otherwise.reverse_otherwise.exec({ as: alice })
      //      const res_after = await reverse_otherwise.reverse_otherwise.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('reverse_with_enum', async () => {
      await reverse_with_enum.reverse_with_enum.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await reverse_with_enum.reverse_with_enum.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await reverse_with_enum.reverse_with_enum.exec({ as: alice })
      //      const res_after = await reverse_with_enum.reverse_with_enum.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('rf_require_otherwise', async () => {
      await rf_require_otherwise.rf_require_otherwise.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await rf_require_otherwise.rf_require_otherwise.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await rf_require_otherwise.rf_require_otherwise.exec({ as: alice })
      //      const res_after = await rf_require_otherwise.rf_require_otherwise.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('same_varname_in_two_distinct_scope', async () => {
      await same_varname_in_two_distinct_scope.same_varname_in_two_distinct_scope.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await same_varname_in_two_distinct_scope.same_varname_in_two_distinct_scope.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await same_varname_in_two_distinct_scope.same_varname_in_two_distinct_scope.exec({ as: alice })
      //      const res_after = await same_varname_in_two_distinct_scope.same_varname_in_two_distinct_scope.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('sample_asset_view', async () => {
      await sample_asset_view.sample_asset_view.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await sample_asset_view.sample_asset_view.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await sample_asset_view.sample_asset_view.exec({ as: alice })
      //      const res_after = await sample_asset_view.sample_asset_view.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('sapling_empty_state', async () => {
      await sapling_empty_state.sapling_empty_state.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await sapling_empty_state.sapling_empty_state.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await sapling_empty_state.sapling_empty_state.exec({ as: alice })
      //      const res_after = await sapling_empty_state.sapling_empty_state.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('sapling_var', async () => {
      await sapling_var.sapling_var.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await sapling_var.sapling_var.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await sapling_var.sapling_var.exec({ as: alice })
      //      const res_after = await sapling_var.sapling_var.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('sapling_verify_update', async () => {
      await sapling_verify_update.sapling_verify_update.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await sapling_verify_update.sapling_verify_update.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await sapling_verify_update.sapling_verify_update.exec({ as: alice })
      //      const res_after = await sapling_verify_update.sapling_verify_update.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('section_constant_effect', async () => {
      await section_constant_effect.section_constant_effect.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await section_constant_effect.section_constant_effect.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await section_constant_effect.section_constant_effect.exec({ as: alice })
      //      const res_after = await section_constant_effect.section_constant_effect.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('section_constant_transition', async () => {
      await section_constant_transition.section_constant_transition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await section_constant_transition.section_constant_transition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await section_constant_transition.section_constant_transition.exec({ as: alice })
      //      const res_after = await section_constant_transition.section_constant_transition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('security_pred_no_storage_fail', async () => {
      await security_pred_no_storage_fail.security_pred_no_storage_fail.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await security_pred_no_storage_fail.security_pred_no_storage_fail.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await security_pred_no_storage_fail.security_pred_no_storage_fail.exec({ as: alice })
      //      const res_after = await security_pred_no_storage_fail.security_pred_no_storage_fail.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('security_pred_not_by_role', async () => {
      await security_pred_not_by_role.security_pred_not_by_role.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await security_pred_not_by_role.security_pred_not_by_role.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await security_pred_not_by_role.security_pred_not_by_role.exec({ as: alice })
      //      const res_after = await security_pred_not_by_role.security_pred_not_by_role.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('security_pred_not_by_role_in_entry', async () => {
      await security_pred_not_by_role_in_entry.security_pred_not_by_role_in_entry.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await security_pred_not_by_role_in_entry.security_pred_not_by_role_in_entry.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await security_pred_not_by_role_in_entry.security_pred_not_by_role_in_entry.exec({ as: alice })
      //      const res_after = await security_pred_not_by_role_in_entry.security_pred_not_by_role_in_entry.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('security_pred_not_in_entry', async () => {
      await security_pred_not_in_entry.security_pred_not_in_entry.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await security_pred_not_in_entry.security_pred_not_in_entry.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await security_pred_not_in_entry.security_pred_not_in_entry.exec({ as: alice })
      //      const res_after = await security_pred_not_in_entry.security_pred_not_in_entry.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('security_pred_only_by_role', async () => {
      await security_pred_only_by_role.security_pred_only_by_role.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await security_pred_only_by_role.security_pred_only_by_role.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await security_pred_only_by_role.security_pred_only_by_role.exec({ as: alice })
      //      const res_after = await security_pred_only_by_role.security_pred_only_by_role.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('security_pred_only_by_role_in_entry', async () => {
      await security_pred_only_by_role_in_entry.security_pred_only_by_role_in_entry.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await security_pred_only_by_role_in_entry.security_pred_only_by_role_in_entry.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await security_pred_only_by_role_in_entry.security_pred_only_by_role_in_entry.exec({ as: alice })
      //      const res_after = await security_pred_only_by_role_in_entry.security_pred_only_by_role_in_entry.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('security_pred_only_in_entry', async () => {
      await security_pred_only_in_entry.security_pred_only_in_entry.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await security_pred_only_in_entry.security_pred_only_in_entry.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await security_pred_only_in_entry.security_pred_only_in_entry.exec({ as: alice })
      //      const res_after = await security_pred_only_in_entry.security_pred_only_in_entry.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('security_pred_transferred_by', async () => {
      await security_pred_transferred_by.security_pred_transferred_by.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await security_pred_transferred_by.security_pred_transferred_by.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await security_pred_transferred_by.security_pred_transferred_by.exec({ as: alice })
      //      const res_after = await security_pred_transferred_by.security_pred_transferred_by.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('security_pred_transferred_to', async () => {
      await security_pred_transferred_to.security_pred_transferred_to.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await security_pred_transferred_to.security_pred_transferred_to.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await security_pred_transferred_to.security_pred_transferred_to.exec({ as: alice })
      //      const res_after = await security_pred_transferred_to.security_pred_transferred_to.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('select_partition', async () => {
      await select_partition.select_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await select_partition.select_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await select_partition.select_partition.exec({ as: alice })
      //      const res_after = await select_partition.select_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('select_partition_big_map', async () => {
      await select_partition_big_map.select_partition_big_map.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await select_partition_big_map.select_partition_big_map.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await select_partition_big_map.select_partition_big_map.exec({ as: alice })
      //      const res_after = await select_partition_big_map.select_partition_big_map.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('select_with_extra_var', async () => {
      await select_with_extra_var.select_with_extra_var.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await select_with_extra_var.select_with_extra_var.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await select_with_extra_var.select_with_extra_var.exec({ as: alice })
      //      const res_after = await select_with_extra_var.select_with_extra_var.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('select_with_extra_var2', async () => {
      await select_with_extra_var2.select_with_extra_var2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await select_with_extra_var2.select_with_extra_var2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await select_with_extra_var2.select_with_extra_var2.exec({ as: alice })
      //      const res_after = await select_with_extra_var2.select_with_extra_var2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('select_with_function_in_predicate', async () => {
      await select_with_function_in_predicate.select_with_function_in_predicate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await select_with_function_in_predicate.select_with_function_in_predicate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await select_with_function_in_predicate.select_with_function_in_predicate.exec({ as: alice })
      //      const res_after = await select_with_function_in_predicate.select_with_function_in_predicate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('setdelegate', async () => {
      await setdelegate.setdelegate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await setdelegate.setdelegate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await setdelegate.setdelegate.exec({ as: alice })
      //      const res_after = await setdelegate.setdelegate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('shadow_field', async () => {
      await shadow_field.shadow_field.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await shadow_field.shadow_field.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await shadow_field.shadow_field.exec({ as: alice })
      //      const res_after = await shadow_field.shadow_field.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('shadow_global_var_effect', async () => {
      await shadow_global_var_effect.shadow_global_var_effect.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await shadow_global_var_effect.shadow_global_var_effect.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await shadow_global_var_effect.shadow_global_var_effect.exec({ as: alice })
      //      const res_after = await shadow_global_var_effect.shadow_global_var_effect.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('shadow_var', async () => {
      await shadow_var.shadow_var.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await shadow_var.shadow_var.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await shadow_var.shadow_var.exec({ as: alice })
      //      const res_after = await shadow_var.shadow_var.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('shadow_var_scope', async () => {
      await shadow_var_scope.shadow_var_scope.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await shadow_var_scope.shadow_var_scope.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await shadow_var_scope.shadow_var_scope.exec({ as: alice })
      //      const res_after = await shadow_var_scope.shadow_var_scope.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple', async () => {
      await simple.simple.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple.simple.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple.simple.exec({ as: alice })
      //      const res_after = await simple.simple.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple2', async () => {
      await simple2.simple2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple2.simple2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple2.simple2.exec({ as: alice })
      //      const res_after = await simple2.simple2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple3', async () => {
      await simple3.simple3.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple3.simple3.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple3.simple3.exec({ as: alice })
      //      const res_after = await simple3.simple3.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple4', async () => {
      await simple4.simple4.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple4.simple4.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple4.simple4.exec({ as: alice })
      //      const res_after = await simple4.simple4.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_2vars', async () => {
      await simple_2vars.simple_2vars.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_2vars.simple_2vars.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_2vars.simple_2vars.exec({ as: alice })
      //      const res_after = await simple_2vars.simple_2vars.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_addupdate', async () => {
      await simple_addupdate.simple_addupdate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_addupdate.simple_addupdate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_addupdate.simple_addupdate.exec({ as: alice })
      //      const res_after = await simple_addupdate.simple_addupdate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_addupdate_asset', async () => {
      await simple_addupdate_asset.simple_addupdate_asset.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_addupdate_asset.simple_addupdate_asset.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_addupdate_asset.simple_addupdate_asset.exec({ as: alice })
      //      const res_after = await simple_addupdate_asset.simple_addupdate_asset.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_arg_int', async () => {
      await simple_arg_int.simple_arg_int.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_arg_int.simple_arg_int.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_arg_int.simple_arg_int.exec({ as: alice })
      //      const res_after = await simple_arg_int.simple_arg_int.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_arith', async () => {
      await simple_arith.simple_arith.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_arith.simple_arith.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_arith.simple_arith.exec({ as: alice })
      //      const res_after = await simple_arith.simple_arith.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_asset', async () => {
      await simple_asset.simple_asset.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_asset.simple_asset.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_asset.simple_asset.exec({ as: alice })
      //      const res_after = await simple_asset.simple_asset.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_asset_2', async () => {
      await simple_asset_2.simple_asset_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_asset_2.simple_asset_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_asset_2.simple_asset_2.exec({ as: alice })
      //      const res_after = await simple_asset_2.simple_asset_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_asset_get_asset1_value', async () => {
      await simple_asset_get_asset1_value.simple_asset_get_asset1_value.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_asset_get_asset1_value.simple_asset_get_asset1_value.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_asset_get_asset1_value.simple_asset_get_asset1_value.exec({ as: alice })
      //      const res_after = await simple_asset_get_asset1_value.simple_asset_get_asset1_value.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_asset_get_asset2_value', async () => {
      await simple_asset_get_asset2_value.simple_asset_get_asset2_value.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_asset_get_asset2_value.simple_asset_get_asset2_value.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_asset_get_asset2_value.simple_asset_get_asset2_value.exec({ as: alice })
      //      const res_after = await simple_asset_get_asset2_value.simple_asset_get_asset2_value.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_asset_get_asset2_value2', async () => {
      await simple_asset_get_asset2_value2.simple_asset_get_asset2_value2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_asset_get_asset2_value2.simple_asset_get_asset2_value2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_asset_get_asset2_value2.simple_asset_get_asset2_value2.exec({ as: alice })
      //      const res_after = await simple_asset_get_asset2_value2.simple_asset_get_asset2_value2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_asset_one_field', async () => {
      await simple_asset_one_field.simple_asset_one_field.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_asset_one_field.simple_asset_one_field.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_asset_one_field.simple_asset_one_field.exec({ as: alice })
      //      const res_after = await simple_asset_one_field.simple_asset_one_field.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_asset_skip', async () => {
      await simple_asset_skip.simple_asset_skip.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_asset_skip.simple_asset_skip.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_asset_skip.simple_asset_skip.exec({ as: alice })
      //      const res_after = await simple_asset_skip.simple_asset_skip.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_asset_skip_empty', async () => {
      await simple_asset_skip_empty.simple_asset_skip_empty.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_asset_skip_empty.simple_asset_skip_empty.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_asset_skip_empty.simple_asset_skip_empty.exec({ as: alice })
      //      const res_after = await simple_asset_skip_empty.simple_asset_skip_empty.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_asset_skip_empty_one_field', async () => {
      await simple_asset_skip_empty_one_field.simple_asset_skip_empty_one_field.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_asset_skip_empty_one_field.simple_asset_skip_empty_one_field.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_asset_skip_empty_one_field.simple_asset_skip_empty_one_field.exec({ as: alice })
      //      const res_after = await simple_asset_skip_empty_one_field.simple_asset_skip_empty_one_field.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_asset_skip_one_field', async () => {
      await simple_asset_skip_one_field.simple_asset_skip_one_field.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_asset_skip_one_field.simple_asset_skip_one_field.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_asset_skip_one_field.simple_asset_skip_one_field.exec({ as: alice })
      //      const res_after = await simple_asset_skip_one_field.simple_asset_skip_one_field.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    it('simple_assign1', async () => {
      await simple_assign1.simple_assign1.deploy({ as: alice })
      const n_before = await simple_assign1.simple_assign1.get_n();
      assert(n_before.equals(new Nat(0)), "Invalid Value")
      await simple_assign1.simple_assign1.exec({ as: alice })
      const n_after = await simple_assign1.simple_assign1.get_n();
      assert(n_after.equals(new Nat(1)), "Invalid Value")
    })

    // TODO
    it('simple_assign2', async () => {
      await simple_assign2.simple_assign2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_assign2.simple_assign2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_assign2.simple_assign2.exec({ as: alice })
      //      const res_after = await simple_assign2.simple_assign2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_assign3', async () => {
      await simple_assign3.simple_assign3.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_assign3.simple_assign3.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_assign3.simple_assign3.exec({ as: alice })
      //      const res_after = await simple_assign3.simple_assign3.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_contract_call', async () => {
      await simple_contract_call.simple_contract_call.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_contract_call.simple_contract_call.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_contract_call.simple_contract_call.exec({ as: alice })
      //      const res_after = await simple_contract_call.simple_contract_call.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_freeze', async () => {
      await simple_freeze.simple_freeze.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_freeze.simple_freeze.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_freeze.simple_freeze.exec({ as: alice })
      //      const res_after = await simple_freeze.simple_freeze.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_fun1', async () => {
      await simple_fun1.simple_fun1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_fun1.simple_fun1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_fun1.simple_fun1.exec({ as: alice })
      //      const res_after = await simple_fun1.simple_fun1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_fun2', async () => {
      await simple_fun2.simple_fun2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_fun2.simple_fun2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_fun2.simple_fun2.exec({ as: alice })
      //      const res_after = await simple_fun2.simple_fun2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_fun3', async () => {
      await simple_fun3.simple_fun3.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_fun3.simple_fun3.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_fun3.simple_fun3.exec({ as: alice })
      //      const res_after = await simple_fun3.simple_fun3.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_fun4', async () => {
      await simple_fun4.simple_fun4.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_fun4.simple_fun4.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_fun4.simple_fun4.exec({ as: alice })
      //      const res_after = await simple_fun4.simple_fun4.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_fun5', async () => {
      await simple_fun5.simple_fun5.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_fun5.simple_fun5.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_fun5.simple_fun5.exec({ as: alice })
      //      const res_after = await simple_fun5.simple_fun5.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_fun6', async () => {
      await simple_fun6.simple_fun6.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_fun6.simple_fun6.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_fun6.simple_fun6.exec({ as: alice })
      //      const res_after = await simple_fun6.simple_fun6.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_fun7', async () => {
      await simple_fun7.simple_fun7.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_fun7.simple_fun7.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_fun7.simple_fun7.exec({ as: alice })
      //      const res_after = await simple_fun7.simple_fun7.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_fun8', async () => {
      await simple_fun8.simple_fun8.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_fun8.simple_fun8.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_fun8.simple_fun8.exec({ as: alice })
      //      const res_after = await simple_fun8.simple_fun8.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_fun_alt', async () => {
      await simple_fun_alt.simple_fun_alt.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_fun_alt.simple_fun_alt.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_fun_alt.simple_fun_alt.exec({ as: alice })
      //      const res_after = await simple_fun_alt.simple_fun_alt.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_fun_with_storage', async () => {
      await simple_fun_with_storage.simple_fun_with_storage.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_fun_with_storage.simple_fun_with_storage.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_fun_with_storage.simple_fun_with_storage.exec({ as: alice })
      //      const res_after = await simple_fun_with_storage.simple_fun_with_storage.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_fun_with_storage2', async () => {
      await simple_fun_with_storage2.simple_fun_with_storage2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_fun_with_storage2.simple_fun_with_storage2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_fun_with_storage2.simple_fun_with_storage2.exec({ as: alice })
      //      const res_after = await simple_fun_with_storage2.simple_fun_with_storage2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_fun_with_storage3', async () => {
      await simple_fun_with_storage3.simple_fun_with_storage3.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_fun_with_storage3.simple_fun_with_storage3.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_fun_with_storage3.simple_fun_with_storage3.exec({ as: alice })
      //      const res_after = await simple_fun_with_storage3.simple_fun_with_storage3.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_get_field', async () => {
      await simple_get_field.simple_get_field.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_get_field.simple_get_field.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_get_field.simple_get_field.exec({ as: alice })
      //      const res_after = await simple_get_field.simple_get_field.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_if3', async () => {
      await simple_if3.simple_if3.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_if3.simple_if3.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_if3.simple_if3.exec({ as: alice })
      //      const res_after = await simple_if3.simple_if3.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_multi_entry', async () => {
      await simple_multi_entry.simple_multi_entry.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_multi_entry.simple_multi_entry.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_multi_entry.simple_multi_entry.exec({ as: alice })
      //      const res_after = await simple_multi_entry.simple_multi_entry.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_multi_entry2', async () => {
      await simple_multi_entry2.simple_multi_entry2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_multi_entry2.simple_multi_entry2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_multi_entry2.simple_multi_entry2.exec({ as: alice })
      //      const res_after = await simple_multi_entry2.simple_multi_entry2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_multi_entry3', async () => {
      await simple_multi_entry3.simple_multi_entry3.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_multi_entry3.simple_multi_entry3.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_multi_entry3.simple_multi_entry3.exec({ as: alice })
      //      const res_after = await simple_multi_entry3.simple_multi_entry3.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_op_add', async () => {
      await simple_op_add.simple_op_add.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_op_add.simple_op_add.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_op_add.simple_op_add.exec({ as: alice })
      //      const res_after = await simple_op_add.simple_op_add.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_op_uminus', async () => {
      await simple_op_uminus.simple_op_uminus.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_op_uminus.simple_op_uminus.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_op_uminus.simple_op_uminus.exec({ as: alice })
      //      const res_after = await simple_op_uminus.simple_op_uminus.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_record_assign', async () => {
      await simple_record_assign.simple_record_assign.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_record_assign.simple_record_assign.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_record_assign.simple_record_assign.exec({ as: alice })
      //      const res_after = await simple_record_assign.simple_record_assign.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_record_assign1', async () => {
      await simple_record_assign1.simple_record_assign1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_record_assign1.simple_record_assign1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_record_assign1.simple_record_assign1.exec({ as: alice })
      //      const res_after = await simple_record_assign1.simple_record_assign1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_record_assign2', async () => {
      await simple_record_assign2.simple_record_assign2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_record_assign2.simple_record_assign2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_record_assign2.simple_record_assign2.exec({ as: alice })
      //      const res_after = await simple_record_assign2.simple_record_assign2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_record_lit', async () => {
      await simple_record_lit.simple_record_lit.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_record_lit.simple_record_lit.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_record_lit.simple_record_lit.exec({ as: alice })
      //      const res_after = await simple_record_lit.simple_record_lit.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_record_lit_rev', async () => {
      await simple_record_lit_rev.simple_record_lit_rev.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_record_lit_rev.simple_record_lit_rev.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_record_lit_rev.simple_record_lit_rev.exec({ as: alice })
      //      const res_after = await simple_record_lit_rev.simple_record_lit_rev.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_reverse', async () => {
      await simple_reverse.simple_reverse.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_reverse.simple_reverse.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_reverse.simple_reverse.exec({ as: alice })
      //      const res_after = await simple_reverse.simple_reverse.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_sequence', async () => {
      await simple_sequence.simple_sequence.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_sequence.simple_sequence.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_sequence.simple_sequence.exec({ as: alice })
      //      const res_after = await simple_sequence.simple_sequence.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_sequence_with_arg', async () => {
      await simple_sequence_with_arg.simple_sequence_with_arg.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_sequence_with_arg.simple_sequence_with_arg.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_sequence_with_arg.simple_sequence_with_arg.exec({ as: alice })
      //      const res_after = await simple_sequence_with_arg.simple_sequence_with_arg.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_sequence_with_arg2', async () => {
      await simple_sequence_with_arg2.simple_sequence_with_arg2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_sequence_with_arg2.simple_sequence_with_arg2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_sequence_with_arg2.simple_sequence_with_arg2.exec({ as: alice })
      //      const res_after = await simple_sequence_with_arg2.simple_sequence_with_arg2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_sequence_with_arg_var', async () => {
      await simple_sequence_with_arg_var.simple_sequence_with_arg_var.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_sequence_with_arg_var.simple_sequence_with_arg_var.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_sequence_with_arg_var.simple_sequence_with_arg_var.exec({ as: alice })
      //      const res_after = await simple_sequence_with_arg_var.simple_sequence_with_arg_var.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_while', async () => {
      await simple_while.simple_while.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_while.simple_while.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_while.simple_while.exec({ as: alice })
      //      const res_after = await simple_while.simple_while.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_with_arg_view', async () => {
      await simple_with_arg_view.simple_with_arg_view.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_with_arg_view.simple_with_arg_view.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_with_arg_view.simple_with_arg_view.exec({ as: alice })
      //      const res_after = await simple_with_arg_view.simple_with_arg_view.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_with_type_annot', async () => {
      await simple_with_type_annot.simple_with_type_annot.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_with_type_annot.simple_with_type_annot.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_with_type_annot.simple_with_type_annot.exec({ as: alice })
      //      const res_after = await simple_with_type_annot.simple_with_type_annot.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('simple_with_view', async () => {
      await simple_with_view.simple_with_view.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await simple_with_view.simple_with_view.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await simple_with_view.simple_with_view.exec({ as: alice })
      //      const res_after = await simple_with_view.simple_with_view.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('sourced_by', async () => {
      await sourced_by.sourced_by.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await sourced_by.sourced_by.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await sourced_by.sourced_by.exec({ as: alice })
      //      const res_after = await sourced_by.sourced_by.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('spec_asset', async () => {
      await spec_asset.spec_asset.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await spec_asset.spec_asset.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await spec_asset.spec_asset.exec({ as: alice })
      //      const res_after = await spec_asset.spec_asset.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('spec_definition', async () => {
      await spec_definition.spec_definition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await spec_definition.spec_definition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await spec_definition.spec_definition.exec({ as: alice })
      //      const res_after = await spec_definition.spec_definition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('spec_definition_2', async () => {
      await spec_definition_2.spec_definition_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await spec_definition_2.spec_definition_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await spec_definition_2.spec_definition_2.exec({ as: alice })
      //      const res_after = await spec_definition_2.spec_definition_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('spec_definition_with_param', async () => {
      await spec_definition_with_param.spec_definition_with_param.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await spec_definition_with_param.spec_definition_with_param.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await spec_definition_with_param.spec_definition_with_param.exec({ as: alice })
      //      const res_after = await spec_definition_with_param.spec_definition_with_param.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('spec_entry', async () => {
      await spec_entry.spec_entry.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await spec_entry.spec_entry.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await spec_entry.spec_entry.exec({ as: alice })
      //      const res_after = await spec_entry.spec_entry.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('spec_fail_caller', async () => {
      await spec_fail_caller.spec_fail_caller.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await spec_fail_caller.spec_fail_caller.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await spec_fail_caller.spec_fail_caller.exec({ as: alice })
      //      const res_after = await spec_fail_caller.spec_fail_caller.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('spec_fail_source', async () => {
      await spec_fail_source.spec_fail_source.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await spec_fail_source.spec_fail_source.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await spec_fail_source.spec_fail_source.exec({ as: alice })
      //      const res_after = await spec_fail_source.spec_fail_source.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('spec_full', async () => {
      await spec_full.spec_full.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await spec_full.spec_full.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await spec_full.spec_full.exec({ as: alice })
      //      const res_after = await spec_full.spec_full.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('spec_function', async () => {
      await spec_function.spec_function.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await spec_function.spec_function.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await spec_function.spec_function.exec({ as: alice })
      //      const res_after = await spec_function.spec_function.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('spec_predicate', async () => {
      await spec_predicate.spec_predicate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await spec_predicate.spec_predicate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await spec_predicate.spec_predicate.exec({ as: alice })
      //      const res_after = await spec_predicate.spec_predicate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('spec_variable', async () => {
      await spec_variable.spec_variable.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await spec_variable.spec_variable.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await spec_variable.spec_variable.exec({ as: alice })
      //      const res_after = await spec_variable.spec_variable.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('state_in_effect', async () => {
      await state_in_effect.state_in_effect.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await state_in_effect.state_in_effect.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await state_in_effect.state_in_effect.exec({ as: alice })
      //      const res_after = await state_in_effect.state_in_effect.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('state_is', async () => {
      await state_is.state_is.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await state_is.state_is.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await state_is.state_is.exec({ as: alice })
      //      const res_after = await state_is.state_is.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('state_var', async () => {
      await state_var.state_var.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await state_var.state_var.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await state_var.state_var.exec({ as: alice })
      //      const res_after = await state_var.state_var.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('tern_bool_false', async () => {
      await tern_bool_false.tern_bool_false.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await tern_bool_false.tern_bool_false.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await tern_bool_false.tern_bool_false.exec({ as: alice })
      //      const res_after = await tern_bool_false.tern_bool_false.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('tern_bool_true', async () => {
      await tern_bool_true.tern_bool_true.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await tern_bool_true.tern_bool_true.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await tern_bool_true.tern_bool_true.exec({ as: alice })
      //      const res_after = await tern_bool_true.tern_bool_true.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('tern_opt', async () => {
      await tern_opt.tern_opt.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await tern_opt.tern_opt.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await tern_opt.tern_opt.exec({ as: alice })
      //      const res_after = await tern_opt.tern_opt.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('tern_opt_3', async () => {
      await tern_opt_3.tern_opt_3.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await tern_opt_3.tern_opt_3.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await tern_opt_3.tern_opt_3.exec({ as: alice })
      //      const res_after = await tern_opt_3.tern_opt_3.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test', async () => {
      await test.test.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test.test.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test.test.exec({ as: alice })
      //      const res_after = await test.test.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_add_asset2_with_partition', async () => {
      await test_add_asset2_with_partition.test_add_asset2_with_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_add_asset2_with_partition.test_add_asset2_with_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_add_asset2_with_partition.test_add_asset2_with_partition.exec({ as: alice })
      //      const res_after = await test_add_asset2_with_partition.test_add_asset2_with_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_add_asset_with_aggregate', async () => {
      await test_add_asset_with_aggregate.test_add_asset_with_aggregate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_add_asset_with_aggregate.test_add_asset_with_aggregate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_add_asset_with_aggregate.test_add_asset_with_aggregate.exec({ as: alice })
      //      const res_after = await test_add_asset_with_aggregate.test_add_asset_with_aggregate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_add_asset_with_both', async () => {
      await test_add_asset_with_both.test_add_asset_with_both.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_add_asset_with_both.test_add_asset_with_both.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_add_asset_with_both.test_add_asset_with_both.exec({ as: alice })
      //      const res_after = await test_add_asset_with_both.test_add_asset_with_both.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_add_asset_with_partition', async () => {
      await test_add_asset_with_partition.test_add_asset_with_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_add_asset_with_partition.test_add_asset_with_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_add_asset_with_partition.test_add_asset_with_partition.exec({ as: alice })
      //      const res_after = await test_add_asset_with_partition.test_add_asset_with_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_addfield_aggregate_1', async () => {
      await test_addfield_aggregate_1.test_addfield_aggregate_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_addfield_aggregate_1.test_addfield_aggregate_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_addfield_aggregate_1.test_addfield_aggregate_1.exec({ as: alice })
      //      const res_after = await test_addfield_aggregate_1.test_addfield_aggregate_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_addfield_aggregate_2', async () => {
      await test_addfield_aggregate_2.test_addfield_aggregate_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_addfield_aggregate_2.test_addfield_aggregate_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_addfield_aggregate_2.test_addfield_aggregate_2.exec({ as: alice })
      //      const res_after = await test_addfield_aggregate_2.test_addfield_aggregate_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_addfield_partition_1', async () => {
      await test_addfield_partition_1.test_addfield_partition_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_addfield_partition_1.test_addfield_partition_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_addfield_partition_1.test_addfield_partition_1.exec({ as: alice })
      //      const res_after = await test_addfield_partition_1.test_addfield_partition_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_addfield_partition_2', async () => {
      await test_addfield_partition_2.test_addfield_partition_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_addfield_partition_2.test_addfield_partition_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_addfield_partition_2.test_addfield_partition_2.exec({ as: alice })
      //      const res_after = await test_addfield_partition_2.test_addfield_partition_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_addupdate_0', async () => {
      await test_addupdate_0.test_addupdate_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_addupdate_0.test_addupdate_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_addupdate_0.test_addupdate_0.exec({ as: alice })
      //      const res_after = await test_addupdate_0.test_addupdate_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_addupdate_1', async () => {
      await test_addupdate_1.test_addupdate_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_addupdate_1.test_addupdate_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_addupdate_1.test_addupdate_1.exec({ as: alice })
      //      const res_after = await test_addupdate_1.test_addupdate_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_addupdate_2', async () => {
      await test_addupdate_2.test_addupdate_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_addupdate_2.test_addupdate_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_addupdate_2.test_addupdate_2.exec({ as: alice })
      //      const res_after = await test_addupdate_2.test_addupdate_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_head_agg_0', async () => {
      await test_asset_head_agg_0.test_asset_head_agg_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_head_agg_0.test_asset_head_agg_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_head_agg_0.test_asset_head_agg_0.exec({ as: alice })
      //      const res_after = await test_asset_head_agg_0.test_asset_head_agg_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_head_agg_1', async () => {
      await test_asset_head_agg_1.test_asset_head_agg_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_head_agg_1.test_asset_head_agg_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_head_agg_1.test_asset_head_agg_1.exec({ as: alice })
      //      const res_after = await test_asset_head_agg_1.test_asset_head_agg_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_head_agg_2', async () => {
      await test_asset_head_agg_2.test_asset_head_agg_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_head_agg_2.test_asset_head_agg_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_head_agg_2.test_asset_head_agg_2.exec({ as: alice })
      //      const res_after = await test_asset_head_agg_2.test_asset_head_agg_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_head_coll_0', async () => {
      await test_asset_head_coll_0.test_asset_head_coll_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_head_coll_0.test_asset_head_coll_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_head_coll_0.test_asset_head_coll_0.exec({ as: alice })
      //      const res_after = await test_asset_head_coll_0.test_asset_head_coll_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_head_coll_1', async () => {
      await test_asset_head_coll_1.test_asset_head_coll_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_head_coll_1.test_asset_head_coll_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_head_coll_1.test_asset_head_coll_1.exec({ as: alice })
      //      const res_after = await test_asset_head_coll_1.test_asset_head_coll_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_head_coll_2', async () => {
      await test_asset_head_coll_2.test_asset_head_coll_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_head_coll_2.test_asset_head_coll_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_head_coll_2.test_asset_head_coll_2.exec({ as: alice })
      //      const res_after = await test_asset_head_coll_2.test_asset_head_coll_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_head_view_0', async () => {
      await test_asset_head_view_0.test_asset_head_view_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_head_view_0.test_asset_head_view_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_head_view_0.test_asset_head_view_0.exec({ as: alice })
      //      const res_after = await test_asset_head_view_0.test_asset_head_view_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_head_view_1', async () => {
      await test_asset_head_view_1.test_asset_head_view_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_head_view_1.test_asset_head_view_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_head_view_1.test_asset_head_view_1.exec({ as: alice })
      //      const res_after = await test_asset_head_view_1.test_asset_head_view_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_head_view_2', async () => {
      await test_asset_head_view_2.test_asset_head_view_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_head_view_2.test_asset_head_view_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_head_view_2.test_asset_head_view_2.exec({ as: alice })
      //      const res_after = await test_asset_head_view_2.test_asset_head_view_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_multi_key', async () => {
      await test_asset_multi_key.test_asset_multi_key.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_multi_key.test_asset_multi_key.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_multi_key.test_asset_multi_key.exec({ as: alice })
      //      const res_after = await test_asset_multi_key.test_asset_multi_key.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_multi_key_complex', async () => {
      await test_asset_multi_key_complex.test_asset_multi_key_complex.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_multi_key_complex.test_asset_multi_key_complex.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_multi_key_complex.test_asset_multi_key_complex.exec({ as: alice })
      //      const res_after = await test_asset_multi_key_complex.test_asset_multi_key_complex.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_nth_agg_0', async () => {
      await test_asset_nth_agg_0.test_asset_nth_agg_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_nth_agg_0.test_asset_nth_agg_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_nth_agg_0.test_asset_nth_agg_0.exec({ as: alice })
      //      const res_after = await test_asset_nth_agg_0.test_asset_nth_agg_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_nth_agg_1', async () => {
      await test_asset_nth_agg_1.test_asset_nth_agg_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_nth_agg_1.test_asset_nth_agg_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_nth_agg_1.test_asset_nth_agg_1.exec({ as: alice })
      //      const res_after = await test_asset_nth_agg_1.test_asset_nth_agg_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_nth_agg_2', async () => {
      await test_asset_nth_agg_2.test_asset_nth_agg_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_nth_agg_2.test_asset_nth_agg_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_nth_agg_2.test_asset_nth_agg_2.exec({ as: alice })
      //      const res_after = await test_asset_nth_agg_2.test_asset_nth_agg_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_nth_coll_0', async () => {
      await test_asset_nth_coll_0.test_asset_nth_coll_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_nth_coll_0.test_asset_nth_coll_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_nth_coll_0.test_asset_nth_coll_0.exec({ as: alice })
      //      const res_after = await test_asset_nth_coll_0.test_asset_nth_coll_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_nth_coll_1', async () => {
      await test_asset_nth_coll_1.test_asset_nth_coll_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_nth_coll_1.test_asset_nth_coll_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_nth_coll_1.test_asset_nth_coll_1.exec({ as: alice })
      //      const res_after = await test_asset_nth_coll_1.test_asset_nth_coll_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_nth_coll_2', async () => {
      await test_asset_nth_coll_2.test_asset_nth_coll_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_nth_coll_2.test_asset_nth_coll_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_nth_coll_2.test_asset_nth_coll_2.exec({ as: alice })
      //      const res_after = await test_asset_nth_coll_2.test_asset_nth_coll_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_nth_view_0', async () => {
      await test_asset_nth_view_0.test_asset_nth_view_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_nth_view_0.test_asset_nth_view_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_nth_view_0.test_asset_nth_view_0.exec({ as: alice })
      //      const res_after = await test_asset_nth_view_0.test_asset_nth_view_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_nth_view_1', async () => {
      await test_asset_nth_view_1.test_asset_nth_view_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_nth_view_1.test_asset_nth_view_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_nth_view_1.test_asset_nth_view_1.exec({ as: alice })
      //      const res_after = await test_asset_nth_view_1.test_asset_nth_view_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_nth_view_2', async () => {
      await test_asset_nth_view_2.test_asset_nth_view_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_nth_view_2.test_asset_nth_view_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_nth_view_2.test_asset_nth_view_2.exec({ as: alice })
      //      const res_after = await test_asset_nth_view_2.test_asset_nth_view_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_select_agg_0', async () => {
      await test_asset_select_agg_0.test_asset_select_agg_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_select_agg_0.test_asset_select_agg_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_select_agg_0.test_asset_select_agg_0.exec({ as: alice })
      //      const res_after = await test_asset_select_agg_0.test_asset_select_agg_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_select_agg_1', async () => {
      await test_asset_select_agg_1.test_asset_select_agg_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_select_agg_1.test_asset_select_agg_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_select_agg_1.test_asset_select_agg_1.exec({ as: alice })
      //      const res_after = await test_asset_select_agg_1.test_asset_select_agg_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_select_agg_2', async () => {
      await test_asset_select_agg_2.test_asset_select_agg_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_select_agg_2.test_asset_select_agg_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_select_agg_2.test_asset_select_agg_2.exec({ as: alice })
      //      const res_after = await test_asset_select_agg_2.test_asset_select_agg_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_select_coll_0', async () => {
      await test_asset_select_coll_0.test_asset_select_coll_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_select_coll_0.test_asset_select_coll_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_select_coll_0.test_asset_select_coll_0.exec({ as: alice })
      //      const res_after = await test_asset_select_coll_0.test_asset_select_coll_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_select_coll_1', async () => {
      await test_asset_select_coll_1.test_asset_select_coll_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_select_coll_1.test_asset_select_coll_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_select_coll_1.test_asset_select_coll_1.exec({ as: alice })
      //      const res_after = await test_asset_select_coll_1.test_asset_select_coll_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_select_coll_2', async () => {
      await test_asset_select_coll_2.test_asset_select_coll_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_select_coll_2.test_asset_select_coll_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_select_coll_2.test_asset_select_coll_2.exec({ as: alice })
      //      const res_after = await test_asset_select_coll_2.test_asset_select_coll_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_select_view_0', async () => {
      await test_asset_select_view_0.test_asset_select_view_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_select_view_0.test_asset_select_view_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_select_view_0.test_asset_select_view_0.exec({ as: alice })
      //      const res_after = await test_asset_select_view_0.test_asset_select_view_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_select_view_1', async () => {
      await test_asset_select_view_1.test_asset_select_view_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_select_view_1.test_asset_select_view_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_select_view_1.test_asset_select_view_1.exec({ as: alice })
      //      const res_after = await test_asset_select_view_1.test_asset_select_view_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_select_view_2', async () => {
      await test_asset_select_view_2.test_asset_select_view_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_select_view_2.test_asset_select_view_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_select_view_2.test_asset_select_view_2.exec({ as: alice })
      //      const res_after = await test_asset_select_view_2.test_asset_select_view_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_sort_agg_0', async () => {
      await test_asset_sort_agg_0.test_asset_sort_agg_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_sort_agg_0.test_asset_sort_agg_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_sort_agg_0.test_asset_sort_agg_0.exec({ as: alice })
      //      const res_after = await test_asset_sort_agg_0.test_asset_sort_agg_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_sort_agg_1', async () => {
      await test_asset_sort_agg_1.test_asset_sort_agg_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_sort_agg_1.test_asset_sort_agg_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_sort_agg_1.test_asset_sort_agg_1.exec({ as: alice })
      //      const res_after = await test_asset_sort_agg_1.test_asset_sort_agg_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_sort_agg_2', async () => {
      await test_asset_sort_agg_2.test_asset_sort_agg_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_sort_agg_2.test_asset_sort_agg_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_sort_agg_2.test_asset_sort_agg_2.exec({ as: alice })
      //      const res_after = await test_asset_sort_agg_2.test_asset_sort_agg_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_sort_coll_0', async () => {
      await test_asset_sort_coll_0.test_asset_sort_coll_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_sort_coll_0.test_asset_sort_coll_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_sort_coll_0.test_asset_sort_coll_0.exec({ as: alice })
      //      const res_after = await test_asset_sort_coll_0.test_asset_sort_coll_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_sort_coll_1', async () => {
      await test_asset_sort_coll_1.test_asset_sort_coll_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_sort_coll_1.test_asset_sort_coll_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_sort_coll_1.test_asset_sort_coll_1.exec({ as: alice })
      //      const res_after = await test_asset_sort_coll_1.test_asset_sort_coll_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_sort_coll_2', async () => {
      await test_asset_sort_coll_2.test_asset_sort_coll_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_sort_coll_2.test_asset_sort_coll_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_sort_coll_2.test_asset_sort_coll_2.exec({ as: alice })
      //      const res_after = await test_asset_sort_coll_2.test_asset_sort_coll_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_sort_coll_complex', async () => {
      await test_asset_sort_coll_complex.test_asset_sort_coll_complex.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_sort_coll_complex.test_asset_sort_coll_complex.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_sort_coll_complex.test_asset_sort_coll_complex.exec({ as: alice })
      //      const res_after = await test_asset_sort_coll_complex.test_asset_sort_coll_complex.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_sort_coll_random', async () => {
      await test_asset_sort_coll_random.test_asset_sort_coll_random.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_sort_coll_random.test_asset_sort_coll_random.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_sort_coll_random.test_asset_sort_coll_random.exec({ as: alice })
      //      const res_after = await test_asset_sort_coll_random.test_asset_sort_coll_random.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_sort_coll_random2', async () => {
      await test_asset_sort_coll_random2.test_asset_sort_coll_random2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_sort_coll_random2.test_asset_sort_coll_random2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_sort_coll_random2.test_asset_sort_coll_random2.exec({ as: alice })
      //      const res_after = await test_asset_sort_coll_random2.test_asset_sort_coll_random2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_sort_coll_rational', async () => {
      await test_asset_sort_coll_rational.test_asset_sort_coll_rational.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_sort_coll_rational.test_asset_sort_coll_rational.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_sort_coll_rational.test_asset_sort_coll_rational.exec({ as: alice })
      //      const res_after = await test_asset_sort_coll_rational.test_asset_sort_coll_rational.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_sort_view_0', async () => {
      await test_asset_sort_view_0.test_asset_sort_view_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_sort_view_0.test_asset_sort_view_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_sort_view_0.test_asset_sort_view_0.exec({ as: alice })
      //      const res_after = await test_asset_sort_view_0.test_asset_sort_view_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_sort_view_1', async () => {
      await test_asset_sort_view_1.test_asset_sort_view_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_sort_view_1.test_asset_sort_view_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_sort_view_1.test_asset_sort_view_1.exec({ as: alice })
      //      const res_after = await test_asset_sort_view_1.test_asset_sort_view_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_sort_view_2', async () => {
      await test_asset_sort_view_2.test_asset_sort_view_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_sort_view_2.test_asset_sort_view_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_sort_view_2.test_asset_sort_view_2.exec({ as: alice })
      //      const res_after = await test_asset_sort_view_2.test_asset_sort_view_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_sum_agg_0', async () => {
      await test_asset_sum_agg_0.test_asset_sum_agg_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_sum_agg_0.test_asset_sum_agg_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_sum_agg_0.test_asset_sum_agg_0.exec({ as: alice })
      //      const res_after = await test_asset_sum_agg_0.test_asset_sum_agg_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_sum_agg_1', async () => {
      await test_asset_sum_agg_1.test_asset_sum_agg_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_sum_agg_1.test_asset_sum_agg_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_sum_agg_1.test_asset_sum_agg_1.exec({ as: alice })
      //      const res_after = await test_asset_sum_agg_1.test_asset_sum_agg_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_sum_agg_2', async () => {
      await test_asset_sum_agg_2.test_asset_sum_agg_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_sum_agg_2.test_asset_sum_agg_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_sum_agg_2.test_asset_sum_agg_2.exec({ as: alice })
      //      const res_after = await test_asset_sum_agg_2.test_asset_sum_agg_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_sum_coll_0', async () => {
      await test_asset_sum_coll_0.test_asset_sum_coll_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_sum_coll_0.test_asset_sum_coll_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_sum_coll_0.test_asset_sum_coll_0.exec({ as: alice })
      //      const res_after = await test_asset_sum_coll_0.test_asset_sum_coll_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_sum_coll_1', async () => {
      await test_asset_sum_coll_1.test_asset_sum_coll_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_sum_coll_1.test_asset_sum_coll_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_sum_coll_1.test_asset_sum_coll_1.exec({ as: alice })
      //      const res_after = await test_asset_sum_coll_1.test_asset_sum_coll_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_sum_coll_2', async () => {
      await test_asset_sum_coll_2.test_asset_sum_coll_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_sum_coll_2.test_asset_sum_coll_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_sum_coll_2.test_asset_sum_coll_2.exec({ as: alice })
      //      const res_after = await test_asset_sum_coll_2.test_asset_sum_coll_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_sum_coll_rat', async () => {
      await test_asset_sum_coll_rat.test_asset_sum_coll_rat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_sum_coll_rat.test_asset_sum_coll_rat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_sum_coll_rat.test_asset_sum_coll_rat.exec({ as: alice })
      //      const res_after = await test_asset_sum_coll_rat.test_asset_sum_coll_rat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_sum_view_0', async () => {
      await test_asset_sum_view_0.test_asset_sum_view_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_sum_view_0.test_asset_sum_view_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_sum_view_0.test_asset_sum_view_0.exec({ as: alice })
      //      const res_after = await test_asset_sum_view_0.test_asset_sum_view_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_sum_view_1', async () => {
      await test_asset_sum_view_1.test_asset_sum_view_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_sum_view_1.test_asset_sum_view_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_sum_view_1.test_asset_sum_view_1.exec({ as: alice })
      //      const res_after = await test_asset_sum_view_1.test_asset_sum_view_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_sum_view_2', async () => {
      await test_asset_sum_view_2.test_asset_sum_view_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_sum_view_2.test_asset_sum_view_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_sum_view_2.test_asset_sum_view_2.exec({ as: alice })
      //      const res_after = await test_asset_sum_view_2.test_asset_sum_view_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_tail_agg_0', async () => {
      await test_asset_tail_agg_0.test_asset_tail_agg_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_tail_agg_0.test_asset_tail_agg_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_tail_agg_0.test_asset_tail_agg_0.exec({ as: alice })
      //      const res_after = await test_asset_tail_agg_0.test_asset_tail_agg_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_tail_agg_1', async () => {
      await test_asset_tail_agg_1.test_asset_tail_agg_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_tail_agg_1.test_asset_tail_agg_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_tail_agg_1.test_asset_tail_agg_1.exec({ as: alice })
      //      const res_after = await test_asset_tail_agg_1.test_asset_tail_agg_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_tail_agg_2', async () => {
      await test_asset_tail_agg_2.test_asset_tail_agg_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_tail_agg_2.test_asset_tail_agg_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_tail_agg_2.test_asset_tail_agg_2.exec({ as: alice })
      //      const res_after = await test_asset_tail_agg_2.test_asset_tail_agg_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_tail_coll_0', async () => {
      await test_asset_tail_coll_0.test_asset_tail_coll_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_tail_coll_0.test_asset_tail_coll_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_tail_coll_0.test_asset_tail_coll_0.exec({ as: alice })
      //      const res_after = await test_asset_tail_coll_0.test_asset_tail_coll_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_tail_coll_1', async () => {
      await test_asset_tail_coll_1.test_asset_tail_coll_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_tail_coll_1.test_asset_tail_coll_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_tail_coll_1.test_asset_tail_coll_1.exec({ as: alice })
      //      const res_after = await test_asset_tail_coll_1.test_asset_tail_coll_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_tail_coll_2', async () => {
      await test_asset_tail_coll_2.test_asset_tail_coll_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_tail_coll_2.test_asset_tail_coll_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_tail_coll_2.test_asset_tail_coll_2.exec({ as: alice })
      //      const res_after = await test_asset_tail_coll_2.test_asset_tail_coll_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_tail_view_0', async () => {
      await test_asset_tail_view_0.test_asset_tail_view_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_tail_view_0.test_asset_tail_view_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_tail_view_0.test_asset_tail_view_0.exec({ as: alice })
      //      const res_after = await test_asset_tail_view_0.test_asset_tail_view_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_tail_view_1', async () => {
      await test_asset_tail_view_1.test_asset_tail_view_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_tail_view_1.test_asset_tail_view_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_tail_view_1.test_asset_tail_view_1.exec({ as: alice })
      //      const res_after = await test_asset_tail_view_1.test_asset_tail_view_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_tail_view_2', async () => {
      await test_asset_tail_view_2.test_asset_tail_view_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_tail_view_2.test_asset_tail_view_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_tail_view_2.test_asset_tail_view_2.exec({ as: alice })
      //      const res_after = await test_asset_tail_view_2.test_asset_tail_view_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_update', async () => {
      await test_asset_update.test_asset_update.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_update.test_asset_update.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_update.test_asset_update.exec({ as: alice })
      //      const res_after = await test_asset_update.test_asset_update.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_update_2', async () => {
      await test_asset_update_2.test_asset_update_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_update_2.test_asset_update_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_update_2.test_asset_update_2.exec({ as: alice })
      //      const res_after = await test_asset_update_2.test_asset_update_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_update_3', async () => {
      await test_asset_update_3.test_asset_update_3.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_update_3.test_asset_update_3.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_update_3.test_asset_update_3.exec({ as: alice })
      //      const res_after = await test_asset_update_3.test_asset_update_3.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_update_4', async () => {
      await test_asset_update_4.test_asset_update_4.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_update_4.test_asset_update_4.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_update_4.test_asset_update_4.exec({ as: alice })
      //      const res_after = await test_asset_update_4.test_asset_update_4.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_update_5', async () => {
      await test_asset_update_5.test_asset_update_5.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_update_5.test_asset_update_5.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_update_5.test_asset_update_5.exec({ as: alice })
      //      const res_after = await test_asset_update_5.test_asset_update_5.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_update_aggregate_1', async () => {
      await test_asset_update_aggregate_1.test_asset_update_aggregate_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_update_aggregate_1.test_asset_update_aggregate_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_update_aggregate_1.test_asset_update_aggregate_1.exec({ as: alice })
      //      const res_after = await test_asset_update_aggregate_1.test_asset_update_aggregate_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_update_aggregate_2', async () => {
      await test_asset_update_aggregate_2.test_asset_update_aggregate_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_update_aggregate_2.test_asset_update_aggregate_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_update_aggregate_2.test_asset_update_aggregate_2.exec({ as: alice })
      //      const res_after = await test_asset_update_aggregate_2.test_asset_update_aggregate_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_update_aggregate_3', async () => {
      await test_asset_update_aggregate_3.test_asset_update_aggregate_3.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_update_aggregate_3.test_asset_update_aggregate_3.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_update_aggregate_3.test_asset_update_aggregate_3.exec({ as: alice })
      //      const res_after = await test_asset_update_aggregate_3.test_asset_update_aggregate_3.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_update_partition_1', async () => {
      await test_asset_update_partition_1.test_asset_update_partition_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_update_partition_1.test_asset_update_partition_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_update_partition_1.test_asset_update_partition_1.exec({ as: alice })
      //      const res_after = await test_asset_update_partition_1.test_asset_update_partition_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_update_partition_2', async () => {
      await test_asset_update_partition_2.test_asset_update_partition_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_update_partition_2.test_asset_update_partition_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_update_partition_2.test_asset_update_partition_2.exec({ as: alice })
      //      const res_after = await test_asset_update_partition_2.test_asset_update_partition_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_update_partition_3', async () => {
      await test_asset_update_partition_3.test_asset_update_partition_3.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_update_partition_3.test_asset_update_partition_3.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_update_partition_3.test_asset_update_partition_3.exec({ as: alice })
      //      const res_after = await test_asset_update_partition_3.test_asset_update_partition_3.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_update_partition_4', async () => {
      await test_asset_update_partition_4.test_asset_update_partition_4.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_update_partition_4.test_asset_update_partition_4.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_update_partition_4.test_asset_update_partition_4.exec({ as: alice })
      //      const res_after = await test_asset_update_partition_4.test_asset_update_partition_4.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_update_partition_5', async () => {
      await test_asset_update_partition_5.test_asset_update_partition_5.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_update_partition_5.test_asset_update_partition_5.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_update_partition_5.test_asset_update_partition_5.exec({ as: alice })
      //      const res_after = await test_asset_update_partition_5.test_asset_update_partition_5.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_update_partition_6', async () => {
      await test_asset_update_partition_6.test_asset_update_partition_6.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_update_partition_6.test_asset_update_partition_6.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_update_partition_6.test_asset_update_partition_6.exec({ as: alice })
      //      const res_after = await test_asset_update_partition_6.test_asset_update_partition_6.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_update_partition_7', async () => {
      await test_asset_update_partition_7.test_asset_update_partition_7.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_update_partition_7.test_asset_update_partition_7.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_update_partition_7.test_asset_update_partition_7.exec({ as: alice })
      //      const res_after = await test_asset_update_partition_7.test_asset_update_partition_7.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_update_partition_8', async () => {
      await test_asset_update_partition_8.test_asset_update_partition_8.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_update_partition_8.test_asset_update_partition_8.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_update_partition_8.test_asset_update_partition_8.exec({ as: alice })
      //      const res_after = await test_asset_update_partition_8.test_asset_update_partition_8.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_update_partition_9', async () => {
      await test_asset_update_partition_9.test_asset_update_partition_9.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_update_partition_9.test_asset_update_partition_9.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_update_partition_9.test_asset_update_partition_9.exec({ as: alice })
      //      const res_after = await test_asset_update_partition_9.test_asset_update_partition_9.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_asset_view', async () => {
      await test_asset_view.test_asset_view.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_asset_view.test_asset_view.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_asset_view.test_asset_view.exec({ as: alice })
      //      const res_after = await test_asset_view.test_asset_view.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_bound_value', async () => {
      await test_bound_value.test_bound_value.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_bound_value.test_bound_value.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_bound_value.test_bound_value.exec({ as: alice })
      //      const res_after = await test_bound_value.test_bound_value.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_caller_getter', async () => {
      await test_caller_getter.test_caller_getter.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_caller_getter.test_caller_getter.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_caller_getter.test_caller_getter.exec({ as: alice })
      //      const res_after = await test_caller_getter.test_caller_getter.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_cmp_bool', async () => {
      await test_cmp_bool.test_cmp_bool.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_cmp_bool.test_cmp_bool.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_cmp_bool.test_cmp_bool.exec({ as: alice })
      //      const res_after = await test_cmp_bool.test_cmp_bool.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_complex_sum', async () => {
      await test_complex_sum.test_complex_sum.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_complex_sum.test_complex_sum.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_complex_sum.test_complex_sum.exec({ as: alice })
      //      const res_after = await test_complex_sum.test_complex_sum.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_conditions', async () => {
      await test_conditions.test_conditions.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_conditions.test_conditions.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_conditions.test_conditions.exec({ as: alice })
      //      const res_after = await test_conditions.test_conditions.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_contains_get', async () => {
      await test_contains_get.test_contains_get.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_contains_get.test_contains_get.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_contains_get.test_contains_get.exec({ as: alice })
      //      const res_after = await test_contains_get.test_contains_get.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_contract', async () => {
      await test_contract.test_contract.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_contract.test_contract.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_contract.test_contract.exec({ as: alice })
      //      const res_after = await test_contract.test_contract.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_contract_self', async () => {
      await test_contract_self.test_contract_self.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_contract_self.test_contract_self.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_contract_self.test_contract_self.exec({ as: alice })
      //      const res_after = await test_contract_self.test_contract_self.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_fget', async () => {
      await test_fget.test_fget.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_fget.test_fget.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_fget.test_fget.exec({ as: alice })
      //      const res_after = await test_fget.test_fget.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_for_list_alt', async () => {
      await test_for_list_alt.test_for_list_alt.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_for_list_alt.test_for_list_alt.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_for_list_alt.test_for_list_alt.exec({ as: alice })
      //      const res_after = await test_for_list_alt.test_for_list_alt.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_fun0', async () => {
      await test_fun0.test_fun0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_fun0.test_fun0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_fun0.test_fun0.exec({ as: alice })
      //      const res_after = await test_fun0.test_fun0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_fun1', async () => {
      await test_fun1.test_fun1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_fun1.test_fun1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_fun1.test_fun1.exec({ as: alice })
      //      const res_after = await test_fun1.test_fun1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_fun2', async () => {
      await test_fun2.test_fun2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_fun2.test_fun2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_fun2.test_fun2.exec({ as: alice })
      //      const res_after = await test_fun2.test_fun2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_fun3', async () => {
      await test_fun3.test_fun3.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_fun3.test_fun3.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_fun3.test_fun3.exec({ as: alice })
      //      const res_after = await test_fun3.test_fun3.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_fun4', async () => {
      await test_fun4.test_fun4.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_fun4.test_fun4.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_fun4.test_fun4.exec({ as: alice })
      //      const res_after = await test_fun4.test_fun4.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_fun5', async () => {
      await test_fun5.test_fun5.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_fun5.test_fun5.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_fun5.test_fun5.exec({ as: alice })
      //      const res_after = await test_fun5.test_fun5.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_fun6', async () => {
      await test_fun6.test_fun6.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_fun6.test_fun6.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_fun6.test_fun6.exec({ as: alice })
      //      const res_after = await test_fun6.test_fun6.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_fun7', async () => {
      await test_fun7.test_fun7.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_fun7.test_fun7.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_fun7.test_fun7.exec({ as: alice })
      //      const res_after = await test_fun7.test_fun7.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_fun8', async () => {
      await test_fun8.test_fun8.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_fun8.test_fun8.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_fun8.test_fun8.exec({ as: alice })
      //      const res_after = await test_fun8.test_fun8.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_fun_asset', async () => {
      await test_fun_asset.test_fun_asset.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_fun_asset.test_fun_asset.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_fun_asset.test_fun_asset.exec({ as: alice })
      //      const res_after = await test_fun_asset.test_fun_asset.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_fun_asset2', async () => {
      await test_fun_asset2.test_fun_asset2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_fun_asset2.test_fun_asset2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_fun_asset2.test_fun_asset2.exec({ as: alice })
      //      const res_after = await test_fun_asset2.test_fun_asset2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_fun_fail', async () => {
      await test_fun_fail.test_fun_fail.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_fun_fail.test_fun_fail.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_fun_fail.test_fun_fail.exec({ as: alice })
      //      const res_after = await test_fun_fail.test_fun_fail.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_getter', async () => {
      await test_getter.test_getter.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_getter.test_getter.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_getter.test_getter.exec({ as: alice })
      //      const res_after = await test_getter.test_getter.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_getter2', async () => {
      await test_getter2.test_getter2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_getter2.test_getter2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_getter2.test_getter2.exec({ as: alice })
      //      const res_after = await test_getter2.test_getter2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_getter_with_arg', async () => {
      await test_getter_with_arg.test_getter_with_arg.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_getter_with_arg.test_getter_with_arg.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_getter_with_arg.test_getter_with_arg.exec({ as: alice })
      //      const res_after = await test_getter_with_arg.test_getter_with_arg.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_getter_with_args', async () => {
      await test_getter_with_args.test_getter_with_args.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_getter_with_args.test_getter_with_args.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_getter_with_args.test_getter_with_args.exec({ as: alice })
      //      const res_after = await test_getter_with_args.test_getter_with_args.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_if_int_nat', async () => {
      await test_if_int_nat.test_if_int_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_if_int_nat.test_if_int_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_if_int_nat.test_if_int_nat.exec({ as: alice })
      //      const res_after = await test_if_int_nat.test_if_int_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_init_asset', async () => {
      await test_init_asset.test_init_asset.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_init_asset.test_init_asset.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_init_asset.test_init_asset.exec({ as: alice })
      //      const res_after = await test_init_asset.test_init_asset.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_init_asset2', async () => {
      await test_init_asset2.test_init_asset2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_init_asset2.test_init_asset2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_init_asset2.test_init_asset2.exec({ as: alice })
      //      const res_after = await test_init_asset2.test_init_asset2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_init_asset3', async () => {
      await test_init_asset3.test_init_asset3.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_init_asset3.test_init_asset3.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_init_asset3.test_init_asset3.exec({ as: alice })
      //      const res_after = await test_init_asset3.test_init_asset3.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_init_rat_with_nat', async () => {
      await test_init_rat_with_nat.test_init_rat_with_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_init_rat_with_nat.test_init_rat_with_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_init_rat_with_nat.test_init_rat_with_nat.exec({ as: alice })
      //      const res_after = await test_init_rat_with_nat.test_init_rat_with_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_init_storage_arith', async () => {
      await test_init_storage_arith.test_init_storage_arith.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_init_storage_arith.test_init_storage_arith.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_init_storage_arith.test_init_storage_arith.exec({ as: alice })
      //      const res_after = await test_init_storage_arith.test_init_storage_arith.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_init_storage_cmp', async () => {
      await test_init_storage_cmp.test_init_storage_cmp.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_init_storage_cmp.test_init_storage_cmp.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_init_storage_cmp.test_init_storage_cmp.exec({ as: alice })
      //      const res_after = await test_init_storage_cmp.test_init_storage_cmp.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_init_storage_funs', async () => {
      await test_init_storage_funs.test_init_storage_funs.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_init_storage_funs.test_init_storage_funs.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_init_storage_funs.test_init_storage_funs.exec({ as: alice })
      //      const res_after = await test_init_storage_funs.test_init_storage_funs.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_init_storage_literals', async () => {
      await test_init_storage_literals.test_init_storage_literals.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_init_storage_literals.test_init_storage_literals.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_init_storage_literals.test_init_storage_literals.exec({ as: alice })
      //      const res_after = await test_init_storage_literals.test_init_storage_literals.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_init_storage_simple', async () => {
      await test_init_storage_simple.test_init_storage_simple.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_init_storage_simple.test_init_storage_simple.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_init_storage_simple.test_init_storage_simple.exec({ as: alice })
      //      const res_after = await test_init_storage_simple.test_init_storage_simple.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_initialized_with_asset', async () => {
      await test_initialized_with_asset.test_initialized_with_asset.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_initialized_with_asset.test_initialized_with_asset.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_initialized_with_asset.test_initialized_with_asset.exec({ as: alice })
      //      const res_after = await test_initialized_with_asset.test_initialized_with_asset.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_initializedby', async () => {
      await test_initializedby.test_initializedby.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_initializedby.test_initializedby.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_initializedby.test_initializedby.exec({ as: alice })
      //      const res_after = await test_initializedby.test_initializedby.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_iter', async () => {
      await test_iter.test_iter.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_iter.test_iter.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_iter.test_iter.exec({ as: alice })
      //      const res_after = await test_iter.test_iter.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_iter2', async () => {
      await test_iter2.test_iter2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_iter2.test_iter2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_iter2.test_iter2.exec({ as: alice })
      //      const res_after = await test_iter2.test_iter2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_length_operations', async () => {
      await test_length_operations.test_length_operations.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_length_operations.test_length_operations.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_length_operations.test_length_operations.exec({ as: alice })
      //      const res_after = await test_length_operations.test_length_operations.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_list_contains', async () => {
      await test_list_contains.test_list_contains.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_list_contains.test_list_contains.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_list_contains.test_list_contains.exec({ as: alice })
      //      const res_after = await test_list_contains.test_list_contains.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_list_contains2', async () => {
      await test_list_contains2.test_list_contains2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_list_contains2.test_list_contains2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_list_contains2.test_list_contains2.exec({ as: alice })
      //      const res_after = await test_list_contains2.test_list_contains2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_list_mynth', async () => {
      await test_list_mynth.test_list_mynth.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_list_mynth.test_list_mynth.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_list_mynth.test_list_mynth.exec({ as: alice })
      //      const res_after = await test_list_mynth.test_list_mynth.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_list_mynth2', async () => {
      await test_list_mynth2.test_list_mynth2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_list_mynth2.test_list_mynth2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_list_mynth2.test_list_mynth2.exec({ as: alice })
      //      const res_after = await test_list_mynth2.test_list_mynth2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_list_mynth3', async () => {
      await test_list_mynth3.test_list_mynth3.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_list_mynth3.test_list_mynth3.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_list_mynth3.test_list_mynth3.exec({ as: alice })
      //      const res_after = await test_list_mynth3.test_list_mynth3.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_list_nth', async () => {
      await test_list_nth.test_list_nth.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_list_nth.test_list_nth.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_list_nth.test_list_nth.exec({ as: alice })
      //      const res_after = await test_list_nth.test_list_nth.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_metadata', async () => {
      await test_metadata.test_metadata.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_metadata.test_metadata.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_metadata.test_metadata.exec({ as: alice })
      //      const res_after = await test_metadata.test_metadata.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_operations', async () => {
      await test_operations.test_operations.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_operations.test_operations.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_operations.test_operations.exec({ as: alice })
      //      const res_after = await test_operations.test_operations.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_oracle', async () => {
      await test_oracle.test_oracle.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_oracle.test_oracle.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_oracle.test_oracle.exec({ as: alice })
      //      const res_after = await test_oracle.test_oracle.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_parameter', async () => {
      await test_parameter.test_parameter.deploy(new Nat(2), "mystr", { as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_parameter.test_parameter.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_parameter.test_parameter.exec({ as: alice })
      //      const res_after = await test_parameter.test_parameter.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_parameter_const', async () => {
      await test_parameter_const.test_parameter_const.deploy(new Nat(2), { as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_parameter_const.test_parameter_const.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_parameter_const.test_parameter_const.exec({ as: alice })
      //      const res_after = await test_parameter_const.test_parameter_const.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_prec', async () => {
      await test_prec.test_prec.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_prec.test_prec.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_prec.test_prec.exec({ as: alice })
      //      const res_after = await test_prec.test_prec.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_quantifiers', async () => {
      await test_quantifiers.test_quantifiers.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_quantifiers.test_quantifiers.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_quantifiers.test_quantifiers.exec({ as: alice })
      //      const res_after = await test_quantifiers.test_quantifiers.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_rational', async () => {
      await test_rational.test_rational.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_rational.test_rational.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_rational.test_rational.exec({ as: alice })
      //      const res_after = await test_rational.test_rational.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_read_asset_after_operation', async () => {
      await test_read_asset_after_operation.test_read_asset_after_operation.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_read_asset_after_operation.test_read_asset_after_operation.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_read_asset_after_operation.test_read_asset_after_operation.exec({ as: alice })
      //      const res_after = await test_read_asset_after_operation.test_read_asset_after_operation.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_read_asset_after_update', async () => {
      await test_read_asset_after_update.test_read_asset_after_update.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_read_asset_after_update.test_read_asset_after_update.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_read_asset_after_update.test_read_asset_after_update.exec({ as: alice })
      //      const res_after = await test_read_asset_after_update.test_read_asset_after_update.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_record', async () => {
      await test_record.test_record.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_record.test_record.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_record.test_record.exec({ as: alice })
      //      const res_after = await test_record.test_record.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_record_access_0', async () => {
      await test_record_access_0.test_record_access_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_record_access_0.test_record_access_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_record_access_0.test_record_access_0.exec({ as: alice })
      //      const res_after = await test_record_access_0.test_record_access_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_record_access_1', async () => {
      await test_record_access_1.test_record_access_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_record_access_1.test_record_access_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_record_access_1.test_record_access_1.exec({ as: alice })
      //      const res_after = await test_record_access_1.test_record_access_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_record_access_2', async () => {
      await test_record_access_2.test_record_access_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_record_access_2.test_record_access_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_record_access_2.test_record_access_2.exec({ as: alice })
      //      const res_after = await test_record_access_2.test_record_access_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_record_access_3', async () => {
      await test_record_access_3.test_record_access_3.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_record_access_3.test_record_access_3.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_record_access_3.test_record_access_3.exec({ as: alice })
      //      const res_after = await test_record_access_3.test_record_access_3.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_record_assign_1', async () => {
      await test_record_assign_1.test_record_assign_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_record_assign_1.test_record_assign_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_record_assign_1.test_record_assign_1.exec({ as: alice })
      //      const res_after = await test_record_assign_1.test_record_assign_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_record_assign_2', async () => {
      await test_record_assign_2.test_record_assign_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_record_assign_2.test_record_assign_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_record_assign_2.test_record_assign_2.exec({ as: alice })
      //      const res_after = await test_record_assign_2.test_record_assign_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_record_assign_3', async () => {
      await test_record_assign_3.test_record_assign_3.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_record_assign_3.test_record_assign_3.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_record_assign_3.test_record_assign_3.exec({ as: alice })
      //      const res_after = await test_record_assign_3.test_record_assign_3.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_record_assign_full', async () => {
      await test_record_assign_full.test_record_assign_full.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_record_assign_full.test_record_assign_full.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_record_assign_full.test_record_assign_full.exec({ as: alice })
      //      const res_after = await test_record_assign_full.test_record_assign_full.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_record_assign_var', async () => {
      await test_record_assign_var.test_record_assign_var.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_record_assign_var.test_record_assign_var.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_record_assign_var.test_record_assign_var.exec({ as: alice })
      //      const res_after = await test_record_assign_var.test_record_assign_var.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_record_simple', async () => {
      await test_record_simple.test_record_simple.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_record_simple.test_record_simple.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_record_simple.test_record_simple.exec({ as: alice })
      //      const res_after = await test_record_simple.test_record_simple.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_remove_asset_with_partition', async () => {
      await test_remove_asset_with_partition.test_remove_asset_with_partition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_remove_asset_with_partition.test_remove_asset_with_partition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_remove_asset_with_partition.test_remove_asset_with_partition.exec({ as: alice })
      //      const res_after = await test_remove_asset_with_partition.test_remove_asset_with_partition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_removeall_aggregate', async () => {
      await test_removeall_aggregate.test_removeall_aggregate.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_removeall_aggregate.test_removeall_aggregate.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_removeall_aggregate.test_removeall_aggregate.exec({ as: alice })
      //      const res_after = await test_removeall_aggregate.test_removeall_aggregate.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_removeall_aggregate_1', async () => {
      await test_removeall_aggregate_1.test_removeall_aggregate_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_removeall_aggregate_1.test_removeall_aggregate_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_removeall_aggregate_1.test_removeall_aggregate_1.exec({ as: alice })
      //      const res_after = await test_removeall_aggregate_1.test_removeall_aggregate_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_removeall_aggregate_2', async () => {
      await test_removeall_aggregate_2.test_removeall_aggregate_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_removeall_aggregate_2.test_removeall_aggregate_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_removeall_aggregate_2.test_removeall_aggregate_2.exec({ as: alice })
      //      const res_after = await test_removeall_aggregate_2.test_removeall_aggregate_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_removeall_partition_1', async () => {
      await test_removeall_partition_1.test_removeall_partition_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_removeall_partition_1.test_removeall_partition_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_removeall_partition_1.test_removeall_partition_1.exec({ as: alice })
      //      const res_after = await test_removeall_partition_1.test_removeall_partition_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_removeall_partition_2', async () => {
      await test_removeall_partition_2.test_removeall_partition_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_removeall_partition_2.test_removeall_partition_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_removeall_partition_2.test_removeall_partition_2.exec({ as: alice })
      //      const res_after = await test_removeall_partition_2.test_removeall_partition_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_removefield_aggregate_1', async () => {
      await test_removefield_aggregate_1.test_removefield_aggregate_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_removefield_aggregate_1.test_removefield_aggregate_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_removefield_aggregate_1.test_removefield_aggregate_1.exec({ as: alice })
      //      const res_after = await test_removefield_aggregate_1.test_removefield_aggregate_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_removefield_aggregate_2', async () => {
      await test_removefield_aggregate_2.test_removefield_aggregate_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_removefield_aggregate_2.test_removefield_aggregate_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_removefield_aggregate_2.test_removefield_aggregate_2.exec({ as: alice })
      //      const res_after = await test_removefield_aggregate_2.test_removefield_aggregate_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_removefield_partition_1', async () => {
      await test_removefield_partition_1.test_removefield_partition_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_removefield_partition_1.test_removefield_partition_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_removefield_partition_1.test_removefield_partition_1.exec({ as: alice })
      //      const res_after = await test_removefield_partition_1.test_removefield_partition_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_removefield_partition_2', async () => {
      await test_removefield_partition_2.test_removefield_partition_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_removefield_partition_2.test_removefield_partition_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_removefield_partition_2.test_removefield_partition_2.exec({ as: alice })
      //      const res_after = await test_removefield_partition_2.test_removefield_partition_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_removeif_agg_0', async () => {
      await test_removeif_agg_0.test_removeif_agg_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_removeif_agg_0.test_removeif_agg_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_removeif_agg_0.test_removeif_agg_0.exec({ as: alice })
      //      const res_after = await test_removeif_agg_0.test_removeif_agg_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_removeif_agg_1', async () => {
      await test_removeif_agg_1.test_removeif_agg_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_removeif_agg_1.test_removeif_agg_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_removeif_agg_1.test_removeif_agg_1.exec({ as: alice })
      //      const res_after = await test_removeif_agg_1.test_removeif_agg_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_removeif_agg_2', async () => {
      await test_removeif_agg_2.test_removeif_agg_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_removeif_agg_2.test_removeif_agg_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_removeif_agg_2.test_removeif_agg_2.exec({ as: alice })
      //      const res_after = await test_removeif_agg_2.test_removeif_agg_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_removeif_coll_0', async () => {
      await test_removeif_coll_0.test_removeif_coll_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_removeif_coll_0.test_removeif_coll_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_removeif_coll_0.test_removeif_coll_0.exec({ as: alice })
      //      const res_after = await test_removeif_coll_0.test_removeif_coll_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_removeif_coll_1', async () => {
      await test_removeif_coll_1.test_removeif_coll_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_removeif_coll_1.test_removeif_coll_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_removeif_coll_1.test_removeif_coll_1.exec({ as: alice })
      //      const res_after = await test_removeif_coll_1.test_removeif_coll_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_removeif_coll_2', async () => {
      await test_removeif_coll_2.test_removeif_coll_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_removeif_coll_2.test_removeif_coll_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_removeif_coll_2.test_removeif_coll_2.exec({ as: alice })
      //      const res_after = await test_removeif_coll_2.test_removeif_coll_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_removeif_part_0', async () => {
      await test_removeif_part_0.test_removeif_part_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_removeif_part_0.test_removeif_part_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_removeif_part_0.test_removeif_part_0.exec({ as: alice })
      //      const res_after = await test_removeif_part_0.test_removeif_part_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_removeif_part_1', async () => {
      await test_removeif_part_1.test_removeif_part_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_removeif_part_1.test_removeif_part_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_removeif_part_1.test_removeif_part_1.exec({ as: alice })
      //      const res_after = await test_removeif_part_1.test_removeif_part_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_removeif_part_2', async () => {
      await test_removeif_part_2.test_removeif_part_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_removeif_part_2.test_removeif_part_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_removeif_part_2.test_removeif_part_2.exec({ as: alice })
      //      const res_after = await test_removeif_part_2.test_removeif_part_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_result', async () => {
      await test_result.test_result.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_result.test_result.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_result.test_result.exec({ as: alice })
      //      const res_after = await test_result.test_result.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_security', async () => {
      await test_security.test_security.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_security.test_security.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_security.test_security.exec({ as: alice })
      //      const res_after = await test_security.test_security.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_specasset', async () => {
      await test_specasset.test_specasset.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_specasset.test_specasset.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_specasset.test_specasset.exec({ as: alice })
      //      const res_after = await test_specasset.test_specasset.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_specfun', async () => {
      await test_specfun.test_specfun.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_specfun.test_specfun.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_specfun.test_specfun.exec({ as: alice })
      //      const res_after = await test_specfun.test_specfun.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_specvar', async () => {
      await test_specvar.test_specvar.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_specvar.test_specvar.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_specvar.test_specvar.exec({ as: alice })
      //      const res_after = await test_specvar.test_specvar.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_tez', async () => {
      await test_tez.test_tez.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_tez.test_tez.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_tez.test_tez.exec({ as: alice })
      //      const res_after = await test_tez.test_tez.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_transfer', async () => {
      await test_transfer.test_transfer.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_transfer.test_transfer.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_transfer.test_transfer.exec({ as: alice })
      //      const res_after = await test_transfer.test_transfer.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_transition', async () => {
      await test_transition.test_transition.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_transition.test_transition.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_transition.test_transition.exec({ as: alice })
      //      const res_after = await test_transition.test_transition.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_tuple_access_1', async () => {
      await test_tuple_access_1.test_tuple_access_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_tuple_access_1.test_tuple_access_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_tuple_access_1.test_tuple_access_1.exec({ as: alice })
      //      const res_after = await test_tuple_access_1.test_tuple_access_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_tuple_access_2', async () => {
      await test_tuple_access_2.test_tuple_access_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_tuple_access_2.test_tuple_access_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_tuple_access_2.test_tuple_access_2.exec({ as: alice })
      //      const res_after = await test_tuple_access_2.test_tuple_access_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_tuple_access_3', async () => {
      await test_tuple_access_3.test_tuple_access_3.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_tuple_access_3.test_tuple_access_3.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_tuple_access_3.test_tuple_access_3.exec({ as: alice })
      //      const res_after = await test_tuple_access_3.test_tuple_access_3.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_update', async () => {
      await test_update.test_update.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_update.test_update.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_update.test_update.exec({ as: alice })
      //      const res_after = await test_update.test_update.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_var', async () => {
      await test_var.test_var.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_var.test_var.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_var.test_var.exec({ as: alice })
      //      const res_after = await test_var.test_var.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('test_voting', async () => {
      await test_voting.test_voting.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await test_voting.test_voting.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await test_voting.test_voting.exec({ as: alice })
      //      const res_after = await test_voting.test_voting.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('timelock', async () => {
      await timelock.timelock.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await timelock.timelock.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await timelock.timelock.exec({ as: alice })
      //      const res_after = await timelock.timelock.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('timelock_var', async () => {
      await timelock_var.timelock_var.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await timelock_var.timelock_var.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await timelock_var.timelock_var.exec({ as: alice })
      //      const res_after = await timelock_var.timelock_var.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('transfer_call', async () => {
      await transfer_call.transfer_call.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await transfer_call.transfer_call.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await transfer_call.transfer_call.exec({ as: alice })
      //      const res_after = await transfer_call.transfer_call.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('transfer_entrypoint', async () => {
      await transfer_entrypoint.transfer_entrypoint.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await transfer_entrypoint.transfer_entrypoint.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await transfer_entrypoint.transfer_entrypoint.exec({ as: alice })
      //      const res_after = await transfer_entrypoint.transfer_entrypoint.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('transfer_entrypoint2', async () => {
      await transfer_entrypoint2.transfer_entrypoint2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await transfer_entrypoint2.transfer_entrypoint2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await transfer_entrypoint2.transfer_entrypoint2.exec({ as: alice })
      //      const res_after = await transfer_entrypoint2.transfer_entrypoint2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('transfer_op', async () => {
      await transfer_op.transfer_op.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await transfer_op.transfer_op.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await transfer_op.transfer_op.exec({ as: alice })
      //      const res_after = await transfer_op.transfer_op.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('transfer_require_entrypoint', async () => {
      await transfer_require_entrypoint.transfer_require_entrypoint.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await transfer_require_entrypoint.transfer_require_entrypoint.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await transfer_require_entrypoint.transfer_require_entrypoint.exec({ as: alice })
      //      const res_after = await transfer_require_entrypoint.transfer_require_entrypoint.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('transfer_self', async () => {
      await transfer_self.transfer_self.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await transfer_self.transfer_self.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await transfer_self.transfer_self.exec({ as: alice })
      //      const res_after = await transfer_self.transfer_self.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('transfer_simple', async () => {
      await transfer_simple.transfer_simple.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await transfer_simple.transfer_simple.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await transfer_simple.transfer_simple.exec({ as: alice })
      //      const res_after = await transfer_simple.transfer_simple.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('transfer_simple_with_entrypoint', async () => {
      await transfer_simple_with_entrypoint.transfer_simple_with_entrypoint.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await transfer_simple_with_entrypoint.transfer_simple_with_entrypoint.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await transfer_simple_with_entrypoint.transfer_simple_with_entrypoint.exec({ as: alice })
      //      const res_after = await transfer_simple_with_entrypoint.transfer_simple_with_entrypoint.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('tuple_in_contains', async () => {
      await tuple_in_contains.tuple_in_contains.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await tuple_in_contains.tuple_in_contains.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await tuple_in_contains.tuple_in_contains.exec({ as: alice })
      //      const res_after = await tuple_in_contains.tuple_in_contains.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('type_or', async () => {
      await type_or.type_or.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await type_or.type_or.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await type_or.type_or.exec({ as: alice })
      //      const res_after = await type_or.type_or.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('type_set_enum_param', async () => {
      await type_set_enum_param.type_set_enum_param.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await type_set_enum_param.type_set_enum_param.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await type_set_enum_param.type_set_enum_param.exec({ as: alice })
      //      const res_after = await type_set_enum_param.type_set_enum_param.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('type_storage_or', async () => {
      await type_storage_or.type_storage_or.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await type_storage_or.type_storage_or.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await type_storage_or.type_storage_or.exec({ as: alice })
      //      const res_after = await type_storage_or.type_storage_or.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('type_tx_rollup_l2_address', async () => {
      await type_tx_rollup_l2_address.type_tx_rollup_l2_address.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await type_tx_rollup_l2_address.type_tx_rollup_l2_address.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await type_tx_rollup_l2_address.type_tx_rollup_l2_address.exec({ as: alice })
      //      const res_after = await type_tx_rollup_l2_address.type_tx_rollup_l2_address.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('typetuple', async () => {
      await typetuple.typetuple.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await typetuple.typetuple.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await typetuple.typetuple.exec({ as: alice })
      //      const res_after = await typetuple.typetuple.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('unused_argument', async () => {
      await unused_argument.unused_argument.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await unused_argument.unused_argument.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await unused_argument.unused_argument.exec({ as: alice })
      //      const res_after = await unused_argument.unused_argument.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('unused_variable', async () => {
      await unused_variable.unused_variable.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await unused_variable.unused_variable.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await unused_variable.unused_variable.exec({ as: alice })
      //      const res_after = await unused_variable.unused_variable.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('update_minus_equal', async () => {
      await update_minus_equal.update_minus_equal.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await update_minus_equal.update_minus_equal.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await update_minus_equal.update_minus_equal.exec({ as: alice })
      //      const res_after = await update_minus_equal.update_minus_equal.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('var_in_spec', async () => {
      await var_in_spec.var_in_spec.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await var_in_spec.var_in_spec.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await var_in_spec.var_in_spec.exec({ as: alice })
      //      const res_after = await var_in_spec.var_in_spec.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('var_in_state_inv', async () => {
      await var_in_state_inv.var_in_state_inv.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await var_in_state_inv.var_in_state_inv.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await var_in_state_inv.var_in_state_inv.exec({ as: alice })
      //      const res_after = await var_in_state_inv.var_in_state_inv.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('var_without_effect', async () => {
      await var_without_effect.var_without_effect.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await var_without_effect.var_without_effect.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await var_without_effect.var_without_effect.exec({ as: alice })
      //      const res_after = await var_without_effect.var_without_effect.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('variable_in_container', async () => {
      await variable_in_container.variable_in_container.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await variable_in_container.variable_in_container.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await variable_in_container.variable_in_container.exec({ as: alice })
      //      const res_after = await variable_in_container.variable_in_container.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('verif_simple', async () => {
      await verif_simple.verif_simple.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await verif_simple.verif_simple.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await verif_simple.verif_simple.exec({ as: alice })
      //      const res_after = await verif_simple.verif_simple.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('very_simple', async () => {
      await very_simple.very_simple.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await very_simple.very_simple.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await very_simple.very_simple.exec({ as: alice })
      //      const res_after = await very_simple.very_simple.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('view_0', async () => {
      await view_0.view_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await view_0.view_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await view_0.view_0.exec({ as: alice })
      //      const res_after = await view_0.view_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('view_all_chain', async () => {
      await view_all_chain.view_all_chain.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await view_all_chain.view_all_chain.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await view_all_chain.view_all_chain.exec({ as: alice })
      //      const res_after = await view_all_chain.view_all_chain.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('view_args_0', async () => {
      await view_args_0.view_args_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await view_args_0.view_args_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await view_args_0.view_args_0.exec({ as: alice })
      //      const res_after = await view_args_0.view_args_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('view_args_1', async () => {
      await view_args_1.view_args_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await view_args_1.view_args_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await view_args_1.view_args_1.exec({ as: alice })
      //      const res_after = await view_args_1.view_args_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('view_args_storage_0', async () => {
      await view_args_storage_0.view_args_storage_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await view_args_storage_0.view_args_storage_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await view_args_storage_0.view_args_storage_0.exec({ as: alice })
      //      const res_after = await view_args_storage_0.view_args_storage_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('view_args_storage_1', async () => {
      await view_args_storage_1.view_args_storage_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await view_args_storage_1.view_args_storage_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await view_args_storage_1.view_args_storage_1.exec({ as: alice })
      //      const res_after = await view_args_storage_1.view_args_storage_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('view_asset', async () => {
      await view_asset.view_asset.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await view_asset.view_asset.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await view_asset.view_asset.exec({ as: alice })
      //      const res_after = await view_asset.view_asset.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('view_exhaustive', async () => {
      await view_exhaustive.view_exhaustive.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await view_exhaustive.view_exhaustive.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await view_exhaustive.view_exhaustive.exec({ as: alice })
      //      const res_after = await view_exhaustive.view_exhaustive.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('view_in_arg', async () => {
      await view_in_arg.view_in_arg.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await view_in_arg.view_in_arg.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await view_in_arg.view_in_arg.exec({ as: alice })
      //      const res_after = await view_in_arg.view_in_arg.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('view_offchain', async () => {
      await view_offchain.view_offchain.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await view_offchain.view_offchain.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await view_offchain.view_offchain.exec({ as: alice })
      //      const res_after = await view_offchain.view_offchain.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('view_offchain_nat', async () => {
      await view_offchain_nat.view_offchain_nat.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await view_offchain_nat.view_offchain_nat.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await view_offchain_nat.view_offchain_nat.exec({ as: alice })
      //      const res_after = await view_offchain_nat.view_offchain_nat.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('view_onchain', async () => {
      await view_onchain.view_onchain.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await view_onchain.view_onchain.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await view_onchain.view_onchain.exec({ as: alice })
      //      const res_after = await view_onchain.view_onchain.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('view_onchain_offchain', async () => {
      await view_onchain_offchain.view_onchain_offchain.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await view_onchain_offchain.view_onchain_offchain.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await view_onchain_offchain.view_onchain_offchain.exec({ as: alice })
      //      const res_after = await view_onchain_offchain.view_onchain_offchain.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('view_simple', async () => {
      await view_simple.view_simple.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await view_simple.view_simple.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await view_simple.view_simple.exec({ as: alice })
      //      const res_after = await view_simple.view_simple.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('view_simple_caller', async () => {
      await view_simple_caller.view_simple_caller.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await view_simple_caller.view_simple_caller.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await view_simple_caller.view_simple_caller.exec({ as: alice })
      //      const res_after = await view_simple_caller.view_simple_caller.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('view_storage_0', async () => {
      await view_storage_0.view_storage_0.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await view_storage_0.view_storage_0.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await view_storage_0.view_storage_0.exec({ as: alice })
      //      const res_after = await view_storage_0.view_storage_0.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('view_storage_1', async () => {
      await view_storage_1.view_storage_1.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await view_storage_1.view_storage_1.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await view_storage_1.view_storage_1.exec({ as: alice })
      //      const res_after = await view_storage_1.view_storage_1.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('view_storage_2', async () => {
      await view_storage_2.view_storage_2.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await view_storage_2.view_storage_2.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await view_storage_2.view_storage_2.exec({ as: alice })
      //      const res_after = await view_storage_2.view_storage_2.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('view_storage_3', async () => {
      await view_storage_3.view_storage_3.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await view_storage_3.view_storage_3.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await view_storage_3.view_storage_3.exec({ as: alice })
      //      const res_after = await view_storage_3.view_storage_3.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('view_storage_4', async () => {
      await view_storage_4.view_storage_4.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await view_storage_4.view_storage_4.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await view_storage_4.view_storage_4.exec({ as: alice })
      //      const res_after = await view_storage_4.view_storage_4.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('view_storage_5', async () => {
      await view_storage_5.view_storage_5.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await view_storage_5.view_storage_5.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await view_storage_5.view_storage_5.exec({ as: alice })
      //      const res_after = await view_storage_5.view_storage_5.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('with_metadata_json', async () => {
      await with_metadata_json.with_metadata_json.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await with_metadata_json.with_metadata_json.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await with_metadata_json.with_metadata_json.exec({ as: alice })
      //      const res_after = await with_metadata_json.with_metadata_json.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('with_metadata_json_with_offchain_view', async () => {
      await with_metadata_json_with_offchain_view.with_metadata_json_with_offchain_view.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await with_metadata_json_with_offchain_view.with_metadata_json_with_offchain_view.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await with_metadata_json_with_offchain_view.with_metadata_json_with_offchain_view.exec({ as: alice })
      //      const res_after = await with_metadata_json_with_offchain_view.with_metadata_json_with_offchain_view.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })

    // TODO
    it('with_metadata_uri', async () => {
      await with_metadata_uri.with_metadata_uri.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await with_metadata_uri.with_metadata_uri.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await with_metadata_uri.with_metadata_uri.exec({ as: alice })
      //      const res_after = await with_metadata_uri.with_metadata_uri.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })
  })
})
