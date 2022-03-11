type kind = Getter of Method.t | Setter of Method.t | Predicate of Method.t | Normal of Method.t

let is_getter (method_ : Method.t) =
  let starts_with_get = String.is_prefix ~prefix:"get" (Method.get_method_name method_)
  and rtntype_is_not_void = not @@ String.equal "void" @@ Method.get_return_type method_ in
  starts_with_get && rtntype_is_not_void && Method.is_udf method_


let is_setter (method_ : Method.t) =
  let starts_with_set = String.is_prefix ~prefix:"set" (Method.get_method_name method_)
  and rtntype_is_void = String.equal "void" @@ Method.get_return_type method_ in
  starts_with_set && rtntype_is_void && Method.is_udf method_


let is_predicate (method_ : Method.t) =
  let starts_with_has_or_is =
    let starts_with_has = String.is_prefix ~prefix:"has" (Method.get_method_name method_)
    and starts_with_is = String.is_prefix ~prefix:"is" (Method.to_string method_) in
    starts_with_has || starts_with_is
  and rtntype_is_bool = String.equal "boolean" @@ Method.get_return_type method_ in
  starts_with_has_or_is && rtntype_is_bool && Method.is_udf method_


let is_normal (method_ : Method.t) =
  (not @@ is_getter method_) && (not @@ is_setter method_) && (not @@ is_predicate method_)
