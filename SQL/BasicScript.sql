select d.ind_name, b.value from public.indicators_nlu_basic b join public.indicators_nlu_dic d on b.ind_id = d.ind_id

select max(b.upload_dt) from public.indicators_nlu_basic b